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
 * `ExternalAuthProviderManifest` from `@wasp.sh/spec`.
 *
 * `UserSignupFieldsRef` stays generic on purpose: the reference the app
 * passes is branded by the app's own spec copy, and naming that type here
 * would pin it to the wrong one. The caller's type flows through untouched.
 */
export type BetterAuthProviderManifest<UserSignupFieldsRef = never> = {
  readonly __waspAuthProviderManifest: true;
  kind: "external";
  contractVersion: 1;
  id: "better-auth";
  server: { package: string };
  routes: { basePath: "/better-auth"; rawBody: true };
  capabilities: string[];
  env: { server: EnvVarRequirement[]; client: EnvVarRequirement[] };
  options: { emailAndPassword: boolean };
  userSignupFields?: UserSignupFieldsRef;
};

/**
 * The configuration accepted by {@link betterAuth}.
 */
export interface BetterAuthConfig<UserSignupFieldsRef = never> {
  /**
   * Enable Better Auth's email-and-password flow.
   *
   * @default true
   */
  emailAndPassword?: boolean;

  /**
   * Populates the app's user entity when Wasp provisions a local user for a
   * Better Auth subject it has not seen before, from the claims the adapter
   * verified (`email`, `name`). Required in practice when the user entity has
   * non-nullable fields.
   */
  userSignupFields?: UserSignupFieldsRef;
}

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
 *   provider: betterAuth({ emailAndPassword: true }),
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
export function betterAuth<UserSignupFieldsRef = never>(
  config?: BetterAuthConfig<UserSignupFieldsRef>,
): BetterAuthProviderManifest<UserSignupFieldsRef> {
  return {
    __waspAuthProviderManifest: true,
    kind: "external",
    contractVersion: 1,
    id: "better-auth",
    server: { package: "@wasp.sh/auth-better-auth/server" },
    routes: { basePath: "/better-auth", rawBody: true },
    capabilities: ["session-revocation"],
    env: {
      server: [{ name: "BETTER_AUTH_SECRET", doc: "openssl rand -base64 32" }],
      client: [],
    },
    // Serializable options, delivered verbatim to the server factory.
    options: { emailAndPassword: config?.emailAndPassword ?? true },
    ...(config?.userSignupFields !== undefined
      ? { userSignupFields: config.userSignupFields }
      : {}),
  };
}
