import { type AuthUser } from "wasp/auth";

export function getName(user: AuthUser): string {
  if (user.identities.email !== null) {
    return user.identities.email.id;
  }

  return "Unknown user";
}
