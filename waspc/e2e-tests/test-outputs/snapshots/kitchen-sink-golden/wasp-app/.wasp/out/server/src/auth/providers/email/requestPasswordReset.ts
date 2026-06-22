import { requestPasswordReset as requestPasswordResetWithEmail } from "@wasp.sh/lib-auth/node";
import { Request, Response } from "express";
import {
  doFakeWork,
  findAuthIdentity,
  rethrowPossibleAuthServiceError,
} from "wasp/auth/utils";
import { GetPasswordResetEmailContentFn } from "wasp/server/auth/email";
import {
  createPasswordResetLink,
  sendPasswordResetEmail,
} from "wasp/server/auth/email/utils";
import type { EmailFromField } from "wasp/server/email/core/types";

export function getRequestPasswordResetRoute({
  fromField,
  clientRoute,
  getPasswordResetEmailContent,
}: {
  fromField: EmailFromField;
  clientRoute: string;
  getPasswordResetEmailContent: GetPasswordResetEmailContentFn;
}) {
  return async function requestPasswordReset(
    req: Request<{ email: string }>,
    res: Response,
  ): Promise<void> {
    const args = req.body ?? {};

    try {
      await requestPasswordResetWithEmail({
        fields: args,
        adapters: {
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
          },
          passwordReset: {
            createPasswordResetLink: (email) =>
              createPasswordResetLink(email, clientRoute),
            sendPasswordResetEmail: ({ email, passwordResetLink }) =>
              sendPasswordResetEmail(email, {
                from: fromField,
                to: email,
                ...getPasswordResetEmailContent({ passwordResetLink }),
              }),
          },
          clock: {
            now: () => new Date(),
          },
          workSimulator: {
            doFakeWork,
          },
        },
      });
    } catch (e: unknown) {
      rethrowPossibleAuthServiceError(e);
      throw e;
    }

    res.json({ success: true });
  };
}
