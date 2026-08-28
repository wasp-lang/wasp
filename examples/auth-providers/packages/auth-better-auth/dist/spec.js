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
 * Declares Better Auth as the app's auth provider.
 *
 * Use it in `main.wasp.ts`:
 *
 * ```ts
 * import { betterAuth } from "@wasp.sh/auth-better-auth/spec";
 *
 * auth: {
 *   userEntity: "User",
 *   onAuthFailedRedirectTo: "/login",
 *   providers: [betterAuth()],  // email/password auth, ready to use
 * }
 * ```
 *
 * Better Auth runs in-process and owns its own tables and HTTP endpoints, so
 * the manifest declares more than Clerk's does:
 *
 * - `routes` mounts Better Auth's endpoints (sign-up, sign-in, OAuth
 *   callbacks) at `/better-auth` on the Wasp server. `rawBody` strips Wasp's
 *   JSON body parser there -- Better Auth's handler reads the raw request
 *   stream, and an already-consumed stream makes every request hang with no
 *   error.
 * - The app's `schema.prisma` must contain the four `BetterAuth*` models the
 *   server adapter configures -- see this package's README for the block to
 *   paste in.
 */
export function betterAuth(config) {
    return {
        __waspAuthProviderManifest: true,
        kind: "external",
        contractVersion: 1,
        id: "external:better-auth",
        server: { package: "@wasp.sh/auth-better-auth/server" },
        routes: { basePath: "/better-auth", rawBody: true },
        capabilities: ["session-revocation"],
        env: {
            server: [{ name: "BETTER_AUTH_SECRET", doc: "openssl rand -base64 32" }],
            client: [],
        },
        ...(config?.userSignupFields !== undefined
            ? { userSignupFields: config.userSignupFields }
            : {}),
        ...(config?.setupFn !== undefined ? { setupFn: config.setupFn } : {}),
    };
}
