/**
 * Runtime-agnostic helpers over the identities Wasp's own auth records.
 * Usable from both the server (`context.user`) and the client (`useAuth()`).
 */
// Each method records its identities under its own provider name. The
// handler's name is the app's choice, so the helpers match on the provider
// name alone.
const EMAIL_PROVIDER_NAME = "email";
const USERNAME_PROVIDER_NAME = "username";
// PUBLIC API
export function getEmail(user) {
    return findIdentity(user, EMAIL_PROVIDER_NAME)?.providerUserId ?? null;
}
// PUBLIC API
export function getUsername(user) {
    return findIdentity(user, USERNAME_PROVIDER_NAME)?.providerUserId ?? null;
}
// `context.user` carries a flat `identities` list; a user row loaded with its
// auth relation carries them under `auth.identities`. Both shapes are served.
function findIdentity(user, providerName) {
    const identities = user.identities ?? user.auth?.identities ?? [];
    return (identities.find((identity) => identity.providerName === providerName) ??
        null);
}
