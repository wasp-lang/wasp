/**
 * Runtime-agnostic helpers over the identities Wasp's own auth records.
 * Usable from both the server (`context.user`) and the client (`useAuth()`).
 */

// Identities live under `<scheme>:<method>`; the scheme name is the app's
// choice, so the helpers match on the method suffix.
const EMAIL_NAMESPACE_SUFFIX = ":email";
const USERNAME_NAMESPACE_SUFFIX = ":username";

type UserWithIdentities = {
  auth?: {
    identities: Array<{ providerName: string; providerUserId: string }>;
  } | null;
  identities?: Array<{ providerName: string; providerUserId: string }>;
};

// PUBLIC API
export function getEmail(user: UserWithIdentities): string | null {
  return findIdentity(user, EMAIL_NAMESPACE_SUFFIX)?.providerUserId ?? null;
}

// PUBLIC API
export function getUsername(user: UserWithIdentities): string | null {
  return findIdentity(user, USERNAME_NAMESPACE_SUFFIX)?.providerUserId ?? null;
}

// `context.user` carries a flat `identities` list; a user row loaded with its
// auth relation carries them under `auth.identities`. Both shapes are served.
function findIdentity(user: UserWithIdentities, namespaceSuffix: string) {
  const identities = user.identities ?? user.auth?.identities ?? [];
  return (
    identities.find((identity) =>
      identity.providerName.endsWith(namespaceSuffix),
    ) ?? null
  );
}
