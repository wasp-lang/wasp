import { betterAuth } from "better-auth";
import { prismaAdapter } from "better-auth/adapters/prisma";
import { bearer } from "better-auth/plugins";
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
export const createServerAuthHandler = (runtime, spec) => {
    // The integration config: everything this handler needs to plug Better Auth
    // into a Wasp app, and nothing about which auth methods exist.
    const integrationConfig = {
        // The app's own PrismaClient, handed over by Wasp. `runtime.db` is typed
        // `unknown` because the client's type is generated per app; Better Auth's
        // handler only needs its dynamic model delegates.
        database: prismaAdapter(runtime.db, {
            provider: runtime.dbProvider,
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
        databaseHooks: withLoginReport(runtime, withEagerProvisioning(runtime, extendedConfig.databaseHooks)),
        user: { ...extendedConfig.user, modelName: "betterAuthUser" },
        session: { ...extendedConfig.session, modelName: "betterAuthSession" },
        account: { ...extendedConfig.account, modelName: "betterAuthAccount" },
        verification: {
            ...extendedConfig.verification,
            modelName: "betterAuthVerification",
        },
        plugins: withBearerPlugin(extendedConfig.plugins),
    });
    const handler = {
        /**
         * Wasp hands every handler a standard web `Request` -- built from the
         * HTTP request, or synthesized with just an `Authorization` header for
         * websocket auth. Better Auth consumes its headers directly either way.
         */
        async authenticate(request) {
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
                    providerUserId: session.user.id,
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
        async signOut(request) {
            await auth.api.signOut({ headers: request.headers }).catch(() => {
                // An already-expired session has nothing to sign out of.
            });
            return Response.json({ success: true });
        },
        /**
         * "Log out every device": Better Auth keeps its sessions in its own
         * table, keyed by its user id, which is the identity's `providerUserId`.
         * Deleted directly through the app's Prisma client, under the model name
         * this handler configured above.
         */
        async signOutEverywhere({ providerUserId }) {
            const db = runtime.db;
            await db.betterAuthSession.deleteMany({
                where: { userId: providerUserId },
            });
        },
    };
    return {
        handler,
        /**
         * Better Auth's own HTTP surface (sign-up, sign-in, sign-out, OAuth
         * callbacks), mounted by Wasp at `/auth/<scheme>`. Better Auth speaks
         * standard `Request` / `Response` natively, so this is its handler as is.
         */
        routeHandler: (request) => auth.handler(request),
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
function withEagerProvisioning(runtime, databaseHooks) {
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
                        identity: { claims: { email: user.email, name: user.name } },
                    });
                },
            },
        },
    };
}
/**
 * Reports every Better Auth login to Wasp, so the app's `onBeforeLogin` and
 * `onAfterLogin` fire for Better Auth users too. Better Auth creates a
 * session row per login; its `before` hook runs first, and a throw there (the
 * app's veto) aborts the session, so a refused login never gets one.
 */
function withLoginReport(runtime, databaseHooks) {
    const existingBeforeSessionCreate = databaseHooks?.session?.create?.before;
    return {
        ...databaseHooks,
        session: {
            ...databaseHooks?.session,
            create: {
                ...databaseHooks?.session?.create,
                before: async (session, context) => {
                    const result = await existingBeforeSessionCreate?.(session, context);
                    if (result === false) {
                        return false;
                    }
                    await runtime.identities.default.reportLogin(session.userId);
                    return result;
                },
            },
        },
    };
}
/** Keeps the bearer plugin present whatever the extension did to `plugins`. */
function withBearerPlugin(plugins) {
    const bearerPlugin = bearer();
    const existingPlugins = plugins ?? [];
    return existingPlugins.some((plugin) => plugin.id === bearerPlugin.id)
        ? existingPlugins
        : [...existingPlugins, bearerPlugin];
}
