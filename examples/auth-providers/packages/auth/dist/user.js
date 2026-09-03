/**
 * Runtime-agnostic helpers over the identities Wasp's own auth records.
 * Usable from both the server (`context.user`) and the client (`useAuth()`).
 */
const EMAIL_NAMESPACE = "wasp:email";
const USERNAME_NAMESPACE = "wasp:username";
// PUBLIC API
export function getEmail(user) {
    return findIdentity(user, EMAIL_NAMESPACE)?.providerUserId ?? null;
}
// PUBLIC API
export function getUsername(user) {
    return findIdentity(user, USERNAME_NAMESPACE)?.providerUserId ?? null;
}
// `context.user` carries a flat `identities` list; a user row loaded with its
// auth relation carries them under `auth.identities`. Both shapes are served.
function findIdentity(user, providerName) {
    const identities = user.identities ?? user.auth?.identities ?? [];
    return (identities.find((identity) => identity.providerName === providerName) ??
        null);
}
