import { HttpError } from "@wasp.sh/lib-sdk-core";
import type { Request, Response } from "express";
import { createInvalidCredentialsError } from "../../../shared/errors.js";
import {
  createProviderId,
  getProviderDataWithPassword,
} from "../../../shared/providerData.js";
import type { AuthHookTypes } from "../../hooks.js";
import type { EmailAuthDependencies } from "./provider.js";

export function verifyEmail<H extends AuthHookTypes>({
  identities,
  hooks,
  jwt,
}: EmailAuthDependencies<H>) {
  return async function verifyEmail(
    req: Request,
    res: Response,
  ): Promise<void> {
    const { token } = req.body;
    const { email } = await jwt
      .validateJWT<{ email: string }>(token)
      .catch(() => {
        throw new HttpError(400, "Email verification failed, invalid token");
      });

    const providerId = createProviderId("email", email);
    const authIdentity = await identities.findAuthIdentity(providerId);
    if (!authIdentity) {
      throw new HttpError(400, "Email verification failed, invalid token");
    }

    const providerData = getProviderDataWithPassword<"email">(
      authIdentity.providerData,
    );

    await identities.updateAuthIdentityProviderData(providerId, providerData, {
      isEmailVerified: true,
    });

    const auth = await identities.findAuthWithUserById(authIdentity.authId);

    if (auth === null) {
      throw createInvalidCredentialsError();
    }

    await hooks.onAfterEmailVerifiedHook({ req, email, user: auth.user });

    res.json({ success: true });
  };
}
