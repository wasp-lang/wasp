import { resetPassword as resetPasswordWithEmail } from "@wasp.sh/lib-auth/node";
import { Request, Response } from "express";
import { validateJWT } from "wasp/auth/jwt";
import {
  findAuthIdentity,
  rethrowPossibleAuthServiceError,
  updateAuthIdentityProviderData,
} from "wasp/auth/utils";

export async function resetPassword(
  req: Request<{ token: string; password: string }>,
  res: Response,
): Promise<void> {
  const args = req.body ?? {};

  try {
    await resetPasswordWithEmail({
      fields: args,
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
      },
    });
  } catch (e: unknown) {
    rethrowPossibleAuthServiceError(e);
    throw e;
  }

  res.json({ success: true });
}
