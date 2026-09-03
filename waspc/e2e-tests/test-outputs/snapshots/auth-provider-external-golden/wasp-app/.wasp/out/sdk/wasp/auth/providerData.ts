// PUBLIC API
/**
 * The name an identity is recorded under: a provider id (`"clerk"`) or one of
 * a provider's namespaces (`"wasp:email"`). Which names exist is the
 * providers' business, so this is a plain string.
 */
export type ProviderName = string

// PUBLIC API
/**
 * ProviderId uniquely identifies an auth identity e.g. the `"wasp:email"`
 * namespace with user id "test@test.com", or the `"clerk"` provider with
 * user id "user_1234567890".
 */
export type ProviderId = {
  providerName: ProviderName;
  providerUserId: string;
}

// PUBLIC API
export function createProviderId(providerName: ProviderName, providerUserId: string): ProviderId {
  return { providerName, providerUserId }
}

// PUBLIC API
/**
 * Parses the `providerData` column (non-secret provider state). Safe to expose:
 * secrets live in a separate column and cannot appear here.
 */
export function parseProviderData<T = Record<string, unknown>>(providerData: string): T {
  // NOTE: We are letting JSON.parse throw an error if the providerData is not valid JSON.
  return JSON.parse(providerData);
}

// PUBLIC API
/**
 * Parses the `providerSecrets` column. Callers get this string only by
 * explicitly opting back into the column the Prisma client omits by default --
 * keep the parsed value on the server.
 */
export function parseProviderSecrets<T = Record<string, unknown>>(providerSecrets: string): T {
  // NOTE: We are letting JSON.parse throw an error if the providerSecrets is not valid JSON.
  return JSON.parse(providerSecrets);
}

// PRIVATE API
export function serializeProviderData(providerData: Record<string, unknown>): string {
  return JSON.stringify(providerData);
}

// PRIVATE API
export function serializeProviderSecrets(providerSecrets: Record<string, unknown>): string {
  return JSON.stringify(providerSecrets);
}
