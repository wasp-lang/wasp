import { Request, Response } from 'express';
import {
    createProviderId,
    findAuthIdentity,
    doFakeWork,
    deserializeAndSanitizeProviderData,
} from "../../utils.js";
import {
    createPasswordResetLinkWithToken,
    sendPasswordResetEmail,
    isEmailResendAllowed,
} from "./utils.js";
import { ensureValidEmail } from "../../validation.js";
import type { EmailFromField } from '../../../email/core/types.js';
import { GetPasswordResetEmailContentFn } from './types.js';
import HttpError from '../../../core/HttpError.js';

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
        req: Request<{ email: string; }>,
        res: Response,
    ): Promise<Response<{ success: true }>> {
        const args = req.body ?? {};
        ensureValidEmail(args);

        const authIdentity = await findAuthIdentity(
            createProviderId("email", args.email),
        );

        // User not found - don't leak information
        if (!authIdentity) {
            await doFakeWork();
            return res.json({ success: true });
        }

        const providerData = deserializeAndSanitizeProviderData<'email'>(authIdentity.providerData);
        // User not verified - don't leak information
        if (!providerData.isEmailVerified) {
            await doFakeWork();
            return res.json({ success: true });
        }

        const { isResendAllowed, timeLeft } = isEmailResendAllowed(providerData, 'passwordResetSentAt');
        if (!isResendAllowed) {
            throw new HttpError(400, `Please wait ${timeLeft} secs before trying again.`);
        }

        const passwordResetLink = await createPasswordResetLinkWithToken(args.email, clientRoute);
        try {
            const email = authIdentity.providerUserId
            await sendPasswordResetEmail(
                email,
                {
                    from: fromField,
                    to: email,
                    ...getPasswordResetEmailContent({ passwordResetLink }),
                },
            );
        } catch (e: any) {
            console.error("Failed to send password reset email:", e);
            throw new HttpError(500, "Failed to send password reset email.");
        }
    
        return res.json({ success: true });
    };
}
