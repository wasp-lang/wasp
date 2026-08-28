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
 * Declares Clerk as the app's auth provider.
 *
 * Use it in `main.wasp.ts`:
 *
 * ```ts
 * import { clerk } from "@wasp.sh/auth-clerk/spec";
 *
 * auth: {
 *   userEntity: "User",
 *   onAuthFailedRedirectTo: "/login",
 *   providers: [clerk()],
 * }
 * ```
 *
 * Clerk contributes no Prisma models and no routes -- the manifest only names
 * the server adapter and the env vars it needs. A missing var fails at boot
 * with its `doc` string as the explanation, not at the first authenticated
 * request.
 */
export function clerk(config) {
    return {
        __waspAuthProviderManifest: true,
        kind: "external",
        contractVersion: 1,
        id: "external:clerk",
        server: { package: "@wasp.sh/auth-clerk/server" },
        client: { package: "@wasp.sh/auth-clerk/client" },
        capabilities: ["session-revocation"],
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
