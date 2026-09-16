/**
 * Non-secret working state the email provider keeps for an identity.
 * Lives in the `providerData` column and may be shown to the user
 * (e.g. in the `identities` view).
 */
export type EmailProviderData = {
    isEmailVerified: boolean;
    emailVerificationSentAt: string | null;
    passwordResetSentAt: string | null;
};
/**
 * Secret material the email provider keeps for an identity. Lives in the
 * `providerSecrets` column, which the Prisma client omits by default -- it
 * never crosses a serialization boundary unless read explicitly.
 */
export type EmailProviderSecrets = {
    hashedPassword: string;
};
export type UsernameProviderData = {};
export type UsernameProviderSecrets = {
    hashedPassword: string;
};
export type OAuthProviderData = {};
export type OAuthProviderSecrets = {};
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
};
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
};
export type ProviderName = keyof PossibleProviderData;
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
};
export declare function createProviderId(providerName: ProviderName, providerUserId: string): ProviderId;
export declare function normalizeProviderUserId(providerName: ProviderName, providerUserId: string): string;
/**
 * Parses the `providerData` column (non-secret provider state). Safe to expose:
 * secrets live in a separate column and cannot appear here.
 */
export declare function parseProviderData<PN extends ProviderName>(providerData: string): PossibleProviderData[PN];
/**
 * Parses the `providerSecrets` column. Callers get this string only by
 * explicitly opting back into the column the Prisma client omits by default --
 * keep the parsed value on the server.
 */
export declare function parseProviderSecrets<PN extends ProviderName>(providerSecrets: string): PossibleProviderSecrets[PN];
export declare function serializeProviderData<PN extends ProviderName>(providerData: PossibleProviderData[PN]): string;
export declare function serializeProviderSecrets<PN extends ProviderName>(providerSecrets: PossibleProviderSecrets[PN]): string;
//# sourceMappingURL=providerData.d.ts.map