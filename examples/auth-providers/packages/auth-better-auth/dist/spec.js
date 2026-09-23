/**
 * The spec constructor: what an app's `main.wasp.ts` imports.
 *
 * This module imports nothing at runtime, and no type from `@wasp.sh/spec`. The
 * app compiles `main.wasp.ts` against its own copy of `@wasp.sh/spec`, and a
 * type that mentioned this package's copy would never be assignable to it
 * (the spec's branded types are unique per copy). So the manifest is
 * constructed and typed structurally here, and the compiler validates it
 * structurally when it reads the app.
 */
/**
 * Declares Better Auth as one of the app's auth schemes.
 *
 * Use it in `main.wasp.ts`:
 *
 * ```ts
 * import { betterAuth } from "@wasp.sh/auth-better-auth/spec";
 *
 * auth: {
 *   userEntity: "User",
 *   onAuthFailedRedirectTo: "/login",
 *   schemes: { "better-auth": betterAuth() },  // email/password auth, ready to use
 * }
 * ```
 *
 * Better Auth issues its own session token and verifies it on every request,
 * so the scheme declares no `credentials`: Wasp issues nothing for it. It
 * runs in-process and owns its own tables and HTTP endpoints, so the
 * manifest declares more than Clerk's does:
 *
 * - `routes` mounts Better Auth's endpoints (sign-up, sign-in, OAuth
 *   callbacks) at `/auth/<scheme>` on the Wasp server, as standard
 *   `Request` / `Response`, which Better Auth speaks natively.
 * - The app's `schema.prisma` must contain the four `BetterAuth*` models the
 *   server auth handler configures -- see this package's README for the block to
 *   paste in.
 */
export function betterAuth(config) {
    return {
        __waspAuthSchemeManifest: true,
        kind: "scheme",
        contractVersion: 19,
        server: {
            authAdapter: { package: "@wasp.sh/auth-better-auth/server" },
            env: [{ name: "BETTER_AUTH_SECRET", doc: "openssl rand -base64 32" }],
            spec: {
                ...(config?.setupFn !== undefined ? { setupFn: config.setupFn } : {}),
            },
            routes: {},
        },
        client: {
            authAdapter: { package: "@wasp.sh/auth-better-auth/client" },
        },
        capabilities: [],
        ...(config?.userSignupFields !== undefined
            ? { userFieldsFromClaims: config.userSignupFields }
            : {}),
    };
}
