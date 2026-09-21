import { betterAuth } from "better-auth";
import { prismaAdapter } from "better-auth/adapters/prisma";
import { toNodeHandler } from "better-auth/node";
import { bearer } from "better-auth/plugins";
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
        async signOut(request) {
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
                        claims: { email: user.email, name: user.name },
                    });
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
