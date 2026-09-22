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

import type { SpecReference } from "@wasp.sh/auth-contract";
import type { BetterAuthSetupFn } from "./server.js";

/**
 * An env var the provider needs. Wasp renders these into the app's generated
 * env validation, so a missing var fails at boot with `doc` as the
 * explanation instead of failing at the first authenticated request.
 */
export type EnvVarRequirement<Name extends string = string> = {
  name: Name;
  doc?: string;
};

/**
 * Computes the app's user fields from the claims Better Auth verified: what the
 * app's `defineUserSignupFields` returned, passed as a
 * `with { type: "ref" }` import.
 */
export type UserFieldsFromClaims = SpecReference<
  Record<string, (data: never) => unknown>
>;

/**
 * The manifest {@link betterAuth} produces, structurally matching
 * `AuthSchemeManifest` from `@wasp.sh/spec`. It is precise on purpose: the
 * adapters derive their types from `typeof betterAuth`
 * (`ServerAuthAdapterFor`).
 */
export type BetterAuthSchemeManifest = {
  readonly __waspAuthSchemeManifest: true;
  kind: "scheme";
  contractVersion: 14;
  server: {
    authAdapter: { package: string };
    env: [EnvVarRequirement<"BETTER_AUTH_SECRET">];
    /** The app's setup function, when given; it arrives live in the adapter. */
    spec: { setupFn?: SpecReference<BetterAuthSetupFn> };
    routes: { rawBody: true };
  };
  client: { authAdapter: { package: string } };
  capabilities: string[];
  userFieldsFromClaims?: UserFieldsFromClaims;
};

/**
 * The configuration accepted by {@link betterAuth}.
 */
export interface BetterAuthConfig {
  /**
   * Populates the app's user entity when Wasp provisions a local user for a
   * Better Auth subject it has not seen before, from the claims the handler
   * verified (`email`, `name`). Required in practice when the user entity has
   * non-nullable fields.
   */
  userSignupFields?: UserFieldsFromClaims;

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
  setupFn?: SpecReference<BetterAuthSetupFn>;
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
export function betterAuth(
  config?: BetterAuthConfig,
): BetterAuthSchemeManifest {
  return {
    __waspAuthSchemeManifest: true,
    kind: "scheme",
    contractVersion: 14,
    server: {
      authAdapter: { package: "@wasp.sh/auth-better-auth/server" },
      env: [{ name: "BETTER_AUTH_SECRET", doc: "openssl rand -base64 32" }],
      spec: {
        ...(config?.setupFn !== undefined ? { setupFn: config.setupFn } : {}),
      },
      routes: { rawBody: true },
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
