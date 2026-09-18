/**
 * The spec helper: what an app's `main.wasp.ts` imports.
 *
 * This module deliberately imports NOTHING -- not even `@wasp.sh/spec`. The
 * app compiles `main.wasp.ts` against its own copy of `@wasp.sh/spec`, and a
 * type that mentioned this package's copy would never be assignable to it
 * (the spec's branded types are unique per copy). So the manifest is
 * constructed and typed structurally here, and the compiler validates it
 * structurally when it reads the app.
 */
/**
 * Declares Clerk as one of the app's auth schemes.
 *
 * Use it in `main.wasp.ts`:
 *
 * ```ts
 * import { clerk } from "@wasp.sh/auth-clerk/spec";
 *
 * auth: {
 *   userEntity: "User",
 *   onAuthFailedRedirectTo: "/login",
 *   schemes: { clerk: clerk() },
 * }
 * ```
 *
 * Clerk's own session token is the credential on every request: the scheme
 * declares no `credentials` of its own, so nothing is issued by Wasp and no
 * table is added. It contributes no Prisma models and no routes -- the
 * manifest only names the server handler, the client auth handler and the env
 * vars they need. A missing var fails at boot with its `doc` string as the
 * explanation, not at the first authenticated request.
 */
export function clerk(config) {
    return {
        __waspAuthSchemeManifest: true,
        kind: "scheme",
        contractVersion: 3,
        handler: "@wasp.sh/auth-clerk",
        server: { package: "@wasp.sh/auth-clerk/server" },
        client: { package: "@wasp.sh/auth-clerk/client" },
        capabilities: [],
        env: {
            server: [
                { name: "CLERK_SECRET_KEY", doc: "Clerk dashboard → API keys" },
                { name: "CLERK_PUBLISHABLE_KEY", doc: "Clerk dashboard → API keys" },
                {
                    name: "CLERK_JWT_KEY",
                    optional: true,
                    doc: "enables networkless JWT verification",
                },
            ],
            client: [
                {
                    name: "REACT_APP_CLERK_PUBLISHABLE_KEY",
                    doc: "Clerk dashboard → API keys (publishable key)",
                },
            ],
        },
        ...(config?.userSignupFields !== undefined
            ? { userSignupFields: config.userSignupFields }
            : {}),
    };
}
