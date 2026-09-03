/**
 * Runtime-agnostic helpers over the identities Wasp's own auth records.
 * Usable from both the server (`context.user`) and the client (`useAuth()`).
 */

const EMAIL_NAMESPACE = "wasp:email";
const USERNAME_NAMESPACE = "wasp:username";

type UserWithIdentities = {
  auth?: {
    identities: Array<{ providerName: string; providerUserId: string }>;
  } | null;
  identities?: Array<{ providerName: string; providerUserId: string }>;
};

// PUBLIC API
export function getEmail(user: UserWithIdentities): string | null {
  return findIdentity(user, EMAIL_NAMESPACE)?.providerUserId ?? null;
}

// PUBLIC API
export function getUsername(user: UserWithIdentities): string | null {
  return findIdentity(user, USERNAME_NAMESPACE)?.providerUserId ?? null;
}

// `context.user` carries a flat `identities` list; a user row loaded with its
// auth relation carries them under `auth.identities`. Both shapes are served.
function findIdentity(user: UserWithIdentities, providerName: string) {
  const identities = user.identities ?? user.auth?.identities ?? [];
  return (
    identities.find((identity) => identity.providerName === providerName) ??
    null
  );
}
