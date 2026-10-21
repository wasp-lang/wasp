import { HttpError } from "@wasp.sh/lib-sdk-core";
import type { EmailFromField } from "@wasp.sh/lib-sdk-core/node";
import type { Request, Response } from "express";
import { isEmailResendAllowed } from "../../../emailResend.js";
import {
  createProviderId,
  getProviderDataWithPassword,
} from "../../../providerData.js";
import { ensureValidEmail } from "../../../validation.js";
import { doFakeWork } from "../../fakeWork.js";
import type { AuthHookTypes } from "../../hooks.js";
import type { EmailAuthDependencies } from "./provider.js";
import type { GetPasswordResetEmailContentFn } from "./types.js";

export function getRequestPasswordResetRoute<H extends AuthHookTypes>(
  {
    fromField,
    clientRoute,
    getPasswordResetEmailContent,
  }: {
    fromField: EmailFromField;
    clientRoute: string;
    getPasswordResetEmailContent: GetPasswordResetEmailContentFn;
  },
  { identities, emailVerification }: EmailAuthDependencies<H>,
) {
  return async function requestPasswordReset(
    req: Request,
    res: Response,
  ): Promise<void> {
    const args = req.body ?? {};
    ensureValidEmail(args);

    const authIdentity = await identities.findAuthIdentity(
      createProviderId("email", args.email),
    );

    /**
     * By doing fake work, we make it harder to enumerate users by measuring
     * the time it takes to respond. If we would respond immediately, an attacker
     * could measure the time it takes to respond and figure out if the user exists.
     */

    if (!authIdentity) {
      await doFakeWork();
      res.json({ success: true });
      return;
    }

    const providerData = getProviderDataWithPassword<"email">(
      authIdentity.providerData,
    );
    const { isResendAllowed, timeLeft } = isEmailResendAllowed(
      providerData,
      "passwordResetSentAt",
    );
    if (!isResendAllowed) {
      throw new HttpError(
        400,
        `Please wait ${timeLeft} secs before trying again.`,
      );
    }

    const passwordResetLink = await emailVerification.createPasswordResetLink(
      args.email,
      clientRoute,
    );
    try {
      const email = authIdentity.providerUserId;
      await emailVerification.sendPasswordResetEmail(email, {
        from: fromField,
        to: email,
        ...getPasswordResetEmailContent({ passwordResetLink }),
      });
    } catch (e: unknown) {
      console.error("Failed to send password reset email:", e);
      throw new HttpError(500, "Failed to send password reset email.");
    }

    res.json({ success: true });
  };
}
