import type { AuthOperationContext, AuthUser } from "wasp/server/module";
import { HttpError } from "wasp/server";

export function requireUser(context: AuthOperationContext): AuthUser {
  if (!context.user) {
    throw new HttpError(401, "Not authenticated");
  }
  return context.user;
}
