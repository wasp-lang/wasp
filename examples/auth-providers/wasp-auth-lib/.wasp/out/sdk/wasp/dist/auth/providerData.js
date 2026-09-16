// PUBLIC API
export function createProviderId(providerName, providerUserId) {
    return {
        providerName,
        providerUserId: normalizeProviderUserId(providerName, providerUserId),
    };
}
// PRIVATE API
export function normalizeProviderUserId(providerName, providerUserId) {
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
            providerName;
            return providerUserId;
    }
}
// PUBLIC API
/**
 * Parses the `providerData` column (non-secret provider state). Safe to expose:
 * secrets live in a separate column and cannot appear here.
 */
export function parseProviderData(providerData) {
    // NOTE: We are letting JSON.parse throw an error if the providerData is not valid JSON.
    return JSON.parse(providerData);
}
// PUBLIC API
/**
 * Parses the `providerSecrets` column. Callers get this string only by
 * explicitly opting back into the column the Prisma client omits by default --
 * keep the parsed value on the server.
 */
export function parseProviderSecrets(providerSecrets) {
    // NOTE: We are letting JSON.parse throw an error if the providerSecrets is not valid JSON.
    return JSON.parse(providerSecrets);
}
// PRIVATE API
export function serializeProviderData(providerData) {
    return JSON.stringify(providerData);
}
// PRIVATE API
export function serializeProviderSecrets(providerSecrets) {
    return JSON.stringify(providerSecrets);
}
//# sourceMappingURL=providerData.js.map