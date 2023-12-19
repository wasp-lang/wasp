import type { User, ProviderName, DeserializedAuthEntity } from './types' 

export function getEmail(user: User): string | null {
  return findUserIdentity(user, "email")?.providerUserId ?? null;
}

export function getUsername(user: User): string | null {
  return findUserIdentity(user, "username")?.providerUserId ?? null;
}

export function findUserIdentity(user: User, providerName: ProviderName): DeserializedAuthEntity | undefined {
  return user.auth.identities.find(
    (identity) => identity.providerName === providerName
  );
}
