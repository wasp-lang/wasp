import type { SanitizedUser as User } from '../_types/index' 

export function getEmail(user: User) {
  return findUserIdentity(user, "email")?.providerUserId ?? null;
}

export function getUsername(user: User) {
  return findUserIdentity(user, "username")?.providerUserId ?? null;
}

export function findUserIdentity(user: User, providerName: string) {
  return user.auth.identities.find(
    (identity) => identity.providerName === providerName
  );
}
