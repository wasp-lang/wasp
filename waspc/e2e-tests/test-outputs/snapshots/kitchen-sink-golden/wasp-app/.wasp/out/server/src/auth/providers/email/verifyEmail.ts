import { verifyEmail as verifyEmailWithToken } from "@wasp.sh/lib-auth/node";
import { Request, Response } from "express";
import { validateJWT } from "wasp/auth/jwt";
import {
  findAuthIdentity,
  findAuthWithUserBy,
  rethrowPossibleAuthServiceError,
  updateAuthIdentityProviderData,
} from "wasp/auth/utils";
import { onAfterEmailVerifiedHook } from "../../hooks.js";

export async function verifyEmail(
  req: Request<{ token: string }>,
  res: Response,
): Promise<void> {
  const fields = req.body ?? {};

  try {
    await verifyEmailWithToken({
      fields,
      request: req,
      adapters: {
        tokenService: {
          verifyEmailToken: validateJWT,
        },
        authRepository: {
          async findIdentity(providerId) {
            const authIdentity = await findAuthIdentity(providerId);
            return authIdentity === null
              ? null
              : {
                  authId: authIdentity.authId,
                  providerName: "email",
                  providerUserId: authIdentity.providerUserId,
                  providerData: authIdentity.providerData,
                };
          },
          async findAuthWithUserByAuthId(authId) {
            const auth = await findAuthWithUserBy({ id: authId });
            return auth === null ? null : { authId: auth.id, user: auth.user };
          },
          async updateIdentityProviderData({
            providerId,
            existingProviderData,
            providerDataUpdates,
          }) {
            const authIdentity = await updateAuthIdentityProviderData(
              providerId,
              existingProviderData,
              providerDataUpdates,
            );
            return {
              authId: authIdentity.authId,
              providerName: "email",
              providerUserId: authIdentity.providerUserId,
              providerData: authIdentity.providerData,
            };
          },
        },
        hooks: {
          onAfterEmailVerified: ({ request, email, user }) =>
            onAfterEmailVerifiedHook({ req: request, email, user }),
        },
      },
    });
  } catch (e: unknown) {
    rethrowPossibleAuthServiceError(e);
    throw e;
  }

  res.json({ success: true });
}
