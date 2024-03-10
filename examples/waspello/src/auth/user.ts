import { getUsername, findUserIdentity, type AuthUser as User } from "wasp/auth";

export function getName(user: User): string {
  // We have two ways of authenticating users, so
  // we have to check which one is used.
  const googleIdentity = findUserIdentity(user, "google");
  const usernameIdentity = findUserIdentity(user, "username");

  if (usernameIdentity) {
    return getUsername(user)!;
  }

  if (googleIdentity) {
    return `Google user ${googleIdentity.providerUserId}`;
  }

  return "Unknown user";
}
