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
 * The manifest {@link clerk} produces, structurally matching
 * `ExternalAuthProviderManifest` from `@wasp.sh/spec`.
 *
 * `UserSignupFieldsRef` stays generic on purpose: the reference the app
 * passes is branded by the app's own spec copy, and naming that type here
 * would pin it to the wrong one. The caller's type flows through untouched.
 */
export type ClerkAuthProviderManifest<UserSignupFieldsRef = never> = {
  readonly __waspAuthProviderManifest: true;
  kind: "external";
  contractVersion: 1;
  id: "clerk";
  server: { package: string };
  client: { package: string };
  capabilities: string[];
  env: { server: EnvVarRequirement[]; client: EnvVarRequirement[] };
  userSignupFields?: UserSignupFieldsRef;
};

/**
 * The configuration accepted by {@link clerk}.
 */
export interface ClerkConfig<UserSignupFieldsRef = never> {
  /**
   * Populates the app's user entity when Wasp provisions a local user for a
   * Clerk subject it has not seen before, from the claims the adapter
   * verified. Required in practice when the user entity has non-nullable
   * fields.
   *
   * NOTE: Clerk's default session token carries no email -- add one to the
   * token template in the Clerk dashboard if the app's user entity needs it
   * at provisioning time.
   */
  userSignupFields?: UserSignupFieldsRef;
}

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
export function clerk<UserSignupFieldsRef = never>(
  config?: ClerkConfig<UserSignupFieldsRef>,
): ClerkAuthProviderManifest<UserSignupFieldsRef> {
  return {
    __waspAuthProviderManifest: true,
    kind: "external",
    contractVersion: 1,
    id: "clerk",
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
