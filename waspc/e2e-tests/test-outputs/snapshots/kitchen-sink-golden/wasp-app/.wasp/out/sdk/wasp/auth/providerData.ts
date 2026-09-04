// PUBLIC API
export type EmailProviderData = {
  hashedPassword: string;
  isEmailVerified: boolean;
  emailVerificationSentAt: string | null;
  passwordResetSentAt: string | null;
}

// PUBLIC API
export type UsernameProviderData = {
  hashedPassword: string;
}

// PUBLIC API
export type OAuthProviderData = {}

// PRIVATE API
/**
 * This type is used for type-level programming e.g. to enumerate
 * all possible provider data types.
 *
 * The keys of this type are the names of the providers and the values
 * are the types of the provider data.
 */
export type PossibleProviderData = {
  email: EmailProviderData;
  username: UsernameProviderData;
  discord: OAuthProviderData;
  slack: OAuthProviderData;
  google: OAuthProviderData;
  keycloak: OAuthProviderData;
  github: OAuthProviderData;
  linkedin: OAuthProviderData;
  microsoft: OAuthProviderData;
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
    case 'linkedin':
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
export function getProviderData<PN extends ProviderName>(
  providerData: string,
):  Omit<PossibleProviderData[PN], 'hashedPassword'> {
  return sanitizeProviderData(getProviderDataWithPassword(providerData));
}

// PUBLIC API
export function getProviderDataWithPassword<PN extends ProviderName>(
  providerData: string,
): PossibleProviderData[PN] {
  // NOTE: We are letting JSON.parse throw an error if the providerData is not valid JSON.
  return JSON.parse(providerData);
}

function sanitizeProviderData<PN extends ProviderName>(
  providerData: PossibleProviderData[PN],
): Omit<PossibleProviderData[PN], 'hashedPassword'> {
  if (providerDataHasPasswordField(providerData)) {
    const { hashedPassword, ...rest } = providerData;
    return rest;
  } else {
    return providerData;
  }
}

// PRIVATE API
export function providerDataHasPasswordField(
  providerData: PossibleProviderData[keyof PossibleProviderData],
): providerData is { hashedPassword: string } {
  return 'hashedPassword' in providerData;
}
