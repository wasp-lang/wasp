/**
 * Runtime-agnostic helpers over the identities Wasp's own auth records.
 * Usable from both the server (`context.user`) and the client (`useAuth()`).
 */
// Identities live under `<scheme>:<method>`; the scheme name is the app's
// choice, so the helpers match on the method suffix.
const EMAIL_PROVIDER_NAME_SUFFIX = ":email";
const USERNAME_PROVIDER_NAME_SUFFIX = ":username";
// PUBLIC API
export function getEmail(user) {
    return findIdentity(user, EMAIL_PROVIDER_NAME_SUFFIX)?.providerUserId ?? null;
}
// PUBLIC API
export function getUsername(user) {
    return (findIdentity(user, USERNAME_PROVIDER_NAME_SUFFIX)?.providerUserId ?? null);
}
// `context.user` carries a flat `identities` list; a user row loaded with its
// auth relation carries them under `auth.identities`. Both shapes are served.
function findIdentity(user, providerNameSuffix) {
    const identities = user.identities ?? user.auth?.identities ?? [];
    return (identities.find((identity) => identity.providerName.endsWith(providerNameSuffix)) ?? null);
}
