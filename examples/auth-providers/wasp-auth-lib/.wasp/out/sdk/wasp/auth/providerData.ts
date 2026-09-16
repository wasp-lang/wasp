// PUBLIC API
/**
 * Non-secret working state the email provider keeps for an identity.
 * Lives in the `providerData` column and may be shown to the user
 * (e.g. in the `identities` view).
 */
export type EmailProviderData = {
  isEmailVerified: boolean;
  emailVerificationSentAt: string | null;
  passwordResetSentAt: string | null;
}

// PUBLIC API
/**
 * Secret material the email provider keeps for an identity. Lives in the
 * `providerSecrets` column, which the Prisma client omits by default -- it
 * never crosses a serialization boundary unless read explicitly.
 */
export type EmailProviderSecrets = {
  hashedPassword: string;
}

// PUBLIC API
export type UsernameProviderData = {}

// PUBLIC API
export type UsernameProviderSecrets = {
  hashedPassword: string;
}

// PUBLIC API
export type OAuthProviderData = {}

// PUBLIC API
export type OAuthProviderSecrets = {}

// PRIVATE API
/**
 * This type is used for type-level programming e.g. to enumerate
 * all possible provider data types.
 *
 * The keys of this type are the names of the providers and the values
 * are the types of the provider's non-secret data.
 */
export type PossibleProviderData = {
  email: EmailProviderData;
  username: UsernameProviderData;
  discord: OAuthProviderData;
  slack: OAuthProviderData;
  google: OAuthProviderData;
  keycloak: OAuthProviderData;
  github: OAuthProviderData;
  microsoft: OAuthProviderData;
}

// PRIVATE API
/**
 * The secret counterpart of `PossibleProviderData`: per-provider shapes of the
 * `providerSecrets` column.
 */
export type PossibleProviderSecrets = {
  email: EmailProviderSecrets;
  username: UsernameProviderSecrets;
  discord: OAuthProviderSecrets;
  slack: OAuthProviderSecrets;
  google: OAuthProviderSecrets;
  keycloak: OAuthProviderSecrets;
  github: OAuthProviderSecrets;
  microsoft: OAuthProviderSecrets;
}

// PUBLIC API
export type ProviderName = keyof PossibleProviderData

// PUBLIC API
/**
 * ProviderId uniquely identifies an auth identity e.g.
 * "email" provider with user id "test@test.com" or
 * "google" provider with user id "1234567890".
 *
 * We use this type to avoid passing the providerName and providerUserId
 * separately. Also, we can normalize the providerUserId to make sure it's
 * consistent across different DB operations.
 */
export type ProviderId = {
  providerName: ProviderName;
  providerUserId: string;
}

// PUBLIC API
export function createProviderId(providerName: ProviderName, providerUserId: string): ProviderId {
  return {
    providerName,
    providerUserId: normalizeProviderUserId(providerName, providerUserId),
  }
}

// PRIVATE API
export function normalizeProviderUserId(providerName: ProviderName, providerUserId: string): string {
  switch (providerName) {
    case 'email':
    case 'username':
      return providerUserId.toLowerCase();
    case 'google':
    case 'github':
    case 'discord':
    case 'keycloak':
    case 'slack':
    case 'microsoft':
      return providerUserId;
    /*
      Why the default case?
      In case users add a new auth provider in the user-land.
      Users can't extend this function because it is private.
      If there is an unknown `providerName` in runtime, we'll
      return the `providerUserId` as is.

      We want to still have explicit OAuth providers listed
      so that we get a type error if we forget to add a new provider
      to the switch statement.
    */
    default:
      providerName satisfies never;
      return providerUserId;
  }
}

// PUBLIC API
/**
 * Parses the `providerData` column (non-secret provider state). Safe to expose:
 * secrets live in a separate column and cannot appear here.
 */
export function parseProviderData<PN extends ProviderName>(
  providerData: string,
): PossibleProviderData[PN] {
  // NOTE: We are letting JSON.parse throw an error if the providerData is not valid JSON.
  return JSON.parse(providerData);
}

// PUBLIC API
/**
 * Parses the `providerSecrets` column. Callers get this string only by
 * explicitly opting back into the column the Prisma client omits by default --
 * keep the parsed value on the server.
 */
export function parseProviderSecrets<PN extends ProviderName>(
  providerSecrets: string,
): PossibleProviderSecrets[PN] {
  // NOTE: We are letting JSON.parse throw an error if the providerSecrets is not valid JSON.
  return JSON.parse(providerSecrets);
}

// PRIVATE API
export function serializeProviderData<PN extends ProviderName>(
  providerData: PossibleProviderData[PN],
): string {
  return JSON.stringify(providerData);
}

// PRIVATE API
export function serializeProviderSecrets<PN extends ProviderName>(
  providerSecrets: PossibleProviderSecrets[PN],
): string {
  return JSON.stringify(providerSecrets);
}
