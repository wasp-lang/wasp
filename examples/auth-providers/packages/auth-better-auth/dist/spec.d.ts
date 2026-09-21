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
 * An env var the provider needs. Wasp renders these into the app's generated
 * env validation, so a missing var fails at boot with `doc` as the
 * explanation instead of failing at the first authenticated request.
 */
export type EnvVarRequirement = {
    name: string;
    optional?: boolean;
    doc?: string;
};
/**
 * The manifest {@link betterAuth} produces, structurally matching
 * `AuthSchemeManifest` from `@wasp.sh/spec`.
 *
 * `UserSignupFieldsRef` stays generic on purpose: the reference the app
 * passes is branded by the app's own spec copy, and naming that type here
 * would pin it to the wrong one. The caller's type flows through untouched.
 */
export type BetterAuthSchemeManifest<UserSignupFieldsRef = never, SetupFnRef = never> = {
    readonly __waspAuthSchemeManifest: true;
    kind: "scheme";
    contractVersion: 6;
    server: {
        authHandlerFactory: {
            package: string;
        };
        env: EnvVarRequirement[];
        /** The app's setup function, when given; it arrives live in the factory. */
        spec: {
            setupFn?: SetupFnRef;
        };
        routes: {
            rawBody: true;
        };
    };
    client: {
        authHandlerFactory: {
            package: string;
        };
    };
    capabilities: string[];
    userFieldsFromClaims?: UserSignupFieldsRef;
};
/**
 * The configuration accepted by {@link betterAuth}.
 */
export interface BetterAuthConfig<UserSignupFieldsRef = never, SetupFnRef = never> {
    /**
     * Populates the app's user entity when Wasp provisions a local user for a
     * Better Auth subject it has not seen before, from the claims the handler
     * verified (`email`, `name`). Required in practice when the user entity has
     * non-nullable fields.
     */
    userSignupFields?: UserSignupFieldsRef;
    /**
     * Setup function for the Better Auth instance, following the same
     * convention as Wasp's `prismaSetupFn`: a reference to a function that
     * receives the handler's integration config (database adapter, secret,
     * base path, table name overrides, bearer transport) and returns the
     * Better Auth options to use.
     *
     * Without it, the handler enables email-and-password auth for you. **With
     * it, nothing is enabled by default** -- the returned configuration is
     * authoritative, with plain Better Auth semantics: enable exactly what you
     * want, exactly as Better Auth's own docs describe (`emailAndPassword`,
     * `socialProviders`, `databaseHooks`, `plugins`, email callbacks, ...).
     *
     * Type it with `BetterAuthSetupFn` from `@wasp.sh/auth-better-auth/server`.
     * The handler re-asserts its load-bearing settings (base path, table name
     * overrides, the bearer plugin, the database adapter) after calling it.
     */
    setupFn?: SetupFnRef;
}
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
 *   callbacks) at `/auth/<scheme>` on the Wasp server. `rawBody` strips
 *   Wasp's JSON body parser there -- Better Auth's handler reads the raw
 *   request stream, and an already-consumed stream makes every request hang
 *   with no error.
 * - The app's `schema.prisma` must contain the four `BetterAuth*` models the
 *   server auth handler configures -- see this package's README for the block to
 *   paste in.
 */
export declare function betterAuth<UserSignupFieldsRef = never, SetupFnRef = never>(config?: BetterAuthConfig<UserSignupFieldsRef, SetupFnRef>): BetterAuthSchemeManifest<UserSignupFieldsRef, SetupFnRef>;
