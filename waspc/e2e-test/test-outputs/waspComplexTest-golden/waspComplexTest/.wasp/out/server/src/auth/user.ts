// Since we can't deduplicate these helper functions in the server and the client
// we have them duplicated in this file and in data/Generator/templates/react-app/src/auth/user.ts
// If you are changing the logic here, make sure to change it there as well.

import type { SanitizedUser as User, ProviderName, DeserializedAuthEntity } from '../_types/index' 

export function getEmail(user: User): string | null {
  return findUserIdentity(user, "email")?.providerUserId ?? null;
}

export function getUsername(user: User): string | null {
  return findUserIdentity(user, "username")?.providerUserId ?? null;
}

export function getFirstProviderUserId(user?: User): string | null {
  if (!user || !user.auth || !user.auth.identities || user.auth.identities.length === 0) {
    return null;
  }

  return user.auth.identities[0].providerUserId ?? null;
}

export function findUserIdentity(user: User, providerName: ProviderName): DeserializedAuthEntity | undefined {
  return user.auth.identities.find(
    (identity) => identity.providerName === providerName
  );
}
