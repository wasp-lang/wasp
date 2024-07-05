import { type AuthUser } from "wasp/auth";

export function getName(user: AuthUser): string {
  if (user.identities.username !== null) {
    return user.identities.username.id;
  }

  if (user.identities.google !== null) {
    return `Google user: ${user.identities.google.id}`;
  }

  return "Unknown user";
}
