import type { User } from './types' 

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
