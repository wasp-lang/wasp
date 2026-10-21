import { HttpError } from "@wasp.sh/lib-sdk-core";
import type { Request, Response } from "express";
import {
  createProviderId,
  getProviderDataWithPassword,
} from "../../../shared/providerData.js";
import {
  ensurePasswordIsPresent,
  ensureTokenIsPresent,
  ensureValidPassword,
} from "../../../shared/validation.js";
import type { AuthHookTypes } from "../../hooks.js";
import type { EmailAuthDependencies } from "./provider.js";

export function resetPassword<H extends AuthHookTypes>({
  identities,
  sessions,
  jwt,
}: EmailAuthDependencies<H>) {
  return async function resetPassword(
    req: Request,
    res: Response,
  ): Promise<void> {
    const args = req.body ?? {};
    // NOTE: The token is validated before the password so that an unauthenticated
    // caller with an invalid token can't learn the deployment's password policy.
    ensureTokenIsPresent(args);

    const { token, password } = args;
    const { email } = await jwt
      .validateJWT<{ email: string }>(token)
      .catch(() => {
        throw new HttpError(400, "Password reset failed, invalid token");
      });

    ensureValidPasswordArg(args);

    const providerId = createProviderId("email", email);
    const authIdentity = await identities.findAuthIdentity(providerId);
    if (!authIdentity) {
      throw new HttpError(400, "Password reset failed, invalid token");
    }

    const providerData = getProviderDataWithPassword<"email">(
      authIdentity.providerData,
    );

    await identities.updateAuthIdentityProviderData(providerId, providerData, {
      // The act of resetting the password verifies the email
      isEmailVerified: true,
      // The password will be hashed when saving the providerData
      // in the DB
      hashedPassword: password,
    });

    // Changing the password invalidates all the existing sessions, so that
    // somebody who got hold of a session can't keep using it.
    await sessions.invalidateAllSessionsForAuthId(authIdentity.authId);

    res.json({ success: true });
  };
}

function ensureValidPasswordArg(args: object): void {
  ensurePasswordIsPresent(args);
  ensureValidPassword(args);
}
