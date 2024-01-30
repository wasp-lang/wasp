import type { AuthUser, ProviderName, DeserializedAuthIdentity } from './types'

export function getEmail(user: AuthUser): string | null {
  return findUserIdentity(user, "email")?.providerUserId ?? null;
}

export function getUsername(user: AuthUser): string | null {
  return findUserIdentity(user, "username")?.providerUserId ?? null;
}

export function getFirstProviderUserId(user?: AuthUser): string | null {
  if (!user || !user.auth || !user.auth.identities || user.auth.identities.length === 0) {
    return null;
  }

  return user.auth.identities[0].providerUserId ?? null;
}

export function findUserIdentity(user: AuthUser, providerName: ProviderName): DeserializedAuthIdentity | undefined {
  return user.auth.identities.find(
    (identity) => identity.providerName === providerName
  );
}
