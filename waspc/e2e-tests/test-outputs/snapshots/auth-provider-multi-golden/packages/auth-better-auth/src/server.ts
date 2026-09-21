import type {
  AuthenticateResult,
  AuthHandler,
  AuthResponse,
  ServerAuthAdapterFor,
  WaspServerRuntimeFor,
} from "@wasp.sh/auth-contract";
import { betterAuth, type BetterAuthOptions } from "better-auth";
import { prismaAdapter } from "better-auth/adapters/prisma";
import { toNodeHandler } from "better-auth/node";
import { bearer } from "better-auth/plugins";
import type { betterAuth as betterAuthSpecHelper } from "./spec.js";

/**
 * The type of the `setupFn` an app can reference from its manifest, following
 * the same convention as Wasp's `PrismaSetupFn`: it receives the handler's
 * integration config (database adapter, secret, base URL and path, trusted
 * origins, table name overrides, bearer transport) and returns the Better
 * Auth options to use.
 *
 * The returned configuration is authoritative and has plain Better Auth
 * semantics: **nothing is enabled unless you enable it** -- `emailAndPassword`,
 * `socialProviders`, `databaseHooks`, `plugins`, email callbacks, exactly as
 * Better Auth's own documentation describes. Spread the received config to
 * keep the integration settings, then add yours.
 *
 * The handler re-asserts its load-bearing settings after calling it (base
 * path, `modelName` overrides, the bearer plugin, the database adapter), so
 * those cannot be broken from here -- everything else is yours.
 */
export type BetterAuthSetupFn = (
  config: BetterAuthOptions,
) => BetterAuthOptions;

/**
 * Better Auth, expressed as a Wasp `AuthHandler`.
 *
 * One adapter builds both the Better Auth instance and the handler that
 * verifies against it, so they are guaranteed to share one configuration --
 * the `ServerAuthHandlerParts` shape exists to make the alternative unrepresentable.
 * Better Auth's own session token is the credential on every request (the
 * client auth handler stores it and Wasp attaches it), so Wasp issues nothing.
 *
 * Two settings on the instance are load-bearing for this integration:
 *
 * - `bearer()` -- Wasp sends the session token in an `Authorization: Bearer`
 *   header rather than a cookie, so the same client code works from a browser,
 *   a native app or a script. Better Auth supports that through this plugin.
 *
 * - `modelName` on every model -- Better Auth's default table names (`user`,
 *   `session`, `account`) would collide with Wasp's own generated tables. Note
 *   these must be the *Prisma client property*, not the `@@map` name: the
 *   handler does a raw `db[modelName]` lookup with no case transformation.
 */
export const createServerAuthHandler: ServerAuthAdapterFor<
  typeof betterAuthSpecHelper
> = (runtime, spec) => {
  // The integration config: everything this handler needs to plug Better Auth
  // into a Wasp app, and nothing about which auth methods exist.
  const integrationConfig: BetterAuthOptions = {
    // The app's own PrismaClient, handed over by Wasp. `runtime.db` is typed
    // `unknown` because the client's type is generated per app; Better Auth's
    // handler only needs its dynamic model delegates.
    database: prismaAdapter(runtime.db as never, {
      provider: runtime.dbProvider as "sqlite",
    }),
    secret: runtime.env.BETTER_AUTH_SECRET,
    baseURL: runtime.serverUrl,
    basePath: runtime.mountPath,
    trustedOrigins: [runtime.clientUrl],

    user: { modelName: "betterAuthUser" },
    session: { modelName: "betterAuthSession" },
    account: { modelName: "betterAuthAccount" },
    verification: { modelName: "betterAuthVerification" },

    plugins: [bearer()],
  };

  // Either the handler's opinionated default or the user's explicit setup --
  // never a mix, so the two can't fight:
  //
  // - No `setupFn`: email-and-password auth is enabled for you. Verification
  //   is off because there is no mail delivery until you wire it.
  // - With a `setupFn`: the function receives the integration config and its
  //   return value is authoritative, with plain Better Auth semantics --
  //   nothing is enabled unless you enable it.
  const setupFn = spec?.setupFn;
  const extendedConfig = setupFn
    ? setupFn(integrationConfig)
    : {
        ...integrationConfig,
        emailAndPassword: { enabled: true, requireEmailVerification: false },
      };

  const auth = betterAuth({
    ...extendedConfig,
    // Re-asserted invariants: without these exact settings the integration
    // breaks (routes are mounted where Wasp mounts the scheme, the table names
    // avoid Wasp's own, the bearer plugin carries the token, and the storage
    // must be the app's database). The extension can change anything else.
    database: integrationConfig.database,
    basePath: runtime.mountPath,
    // Composed, not replaced: the app's own database hooks keep running, and
    // the handler adds the eager-provisioning report on top (see below).
    databaseHooks: withEagerProvisioning(runtime, extendedConfig.databaseHooks),
    user: { ...extendedConfig.user, modelName: "betterAuthUser" },
    session: { ...extendedConfig.session, modelName: "betterAuthSession" },
    account: { ...extendedConfig.account, modelName: "betterAuthAccount" },
    verification: {
      ...extendedConfig.verification,
      modelName: "betterAuthVerification",
    },
    plugins: withBearerPlugin(extendedConfig.plugins),
  });

  const handler: AuthHandler = {
    /**
     * Wasp hands every handler a standard web `Request` -- built from the
     * HTTP request, or synthesized with just an `Authorization` header for
     * websocket auth. Better Auth consumes its headers directly either way.
     */
    async authenticate(request: Request): Promise<AuthenticateResult> {
      const session = await auth.api.getSession({
        headers: request.headers,
      });

      if (!session) {
        return { status: "unauthenticated" };
      }

      return {
        status: "authenticated",
        principal: {
          credentialId: session.session.id,
          subjectId: session.user.id,
          // Verified profile data Wasp records when it provisions the local
          // user.
          claims: {
            email: session.user.email,
            name: session.user.name,
          },
        },
      };
    },

    /**
     * Signs the request's Better Auth session out, through Better Auth's own
     * API: the request carries the session token, which is all `signOut`
     * needs.
     */
    async signOut(request: Request): Promise<AuthResponse> {
      await auth.api.signOut({ headers: request.headers }).catch(() => {
        // An already-expired session has nothing to sign out of.
      });
      return { status: 200, body: { success: true } };
    },
  };

  return {
    handler,

    /**
     * Better Auth's own HTTP surface (sign-up, sign-in, sign-out, OAuth
     * callbacks). Wasp mounts it at `/auth/<scheme>` with the JSON body
     * parser stripped (`rawBody: true`) -- `toNodeHandler` reads the raw
     * request stream, and an already-consumed stream hangs every request.
     */
    routeHandler: toNodeHandler(auth),
  };
};

/**
 * Composes the eager-provisioning report into the app's database hooks,
 * preserving any `user.create.after` the app's `setupFn` declared.
 *
 * Better Auth runs signup in-process, so the handler can observe the exact
 * moment one of its users comes to exist and report it to Wasp -- the local
 * `User` then exists from signup, not from the first authenticated request.
 * The call is idempotent and just-in-time provisioning remains the backstop,
 * so a crash between Better Auth's insert and this report heals on first
 * login.
 */
function withEagerProvisioning(
  runtime: WaspServerRuntimeFor<typeof betterAuthSpecHelper>,
  databaseHooks: BetterAuthOptions["databaseHooks"],
): BetterAuthOptions["databaseHooks"] {
  const existingAfterUserCreate = databaseHooks?.user?.create?.after;
  return {
    ...databaseHooks,
    user: {
      ...databaseHooks?.user,
      create: {
        ...databaseHooks?.user?.create,
        after: async (user, context) => {
          await existingAfterUserCreate?.(user, context);
          // Eager provisioning through the runtime's identity store: the
          // local user exists from the Better Auth signup moment, not from
          // the first login exchange. Idempotent; the exchange's just-in-time
          // provisioning remains the backstop.
          await runtime.identities.default.provision(user.id, {
            claims: { email: user.email, name: user.name },
          });
        },
      },
    },
  };
}

/** Keeps the bearer plugin present whatever the extension did to `plugins`. */
function withBearerPlugin(
  plugins: BetterAuthOptions["plugins"],
): NonNullable<BetterAuthOptions["plugins"]> {
  const bearerPlugin = bearer();
  const existingPlugins = plugins ?? [];
  return existingPlugins.some((plugin) => plugin.id === bearerPlugin.id)
    ? existingPlugins
    : [...existingPlugins, bearerPlugin];
}
