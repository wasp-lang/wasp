import { Request, Response } from 'express';
import {
    createProviderId,
    findAuthIdentity,
    doFakeWork,
    deserializeAndSanitizeProviderData,
} from "../../utils.js";
import {
    createPasswordResetLink,
    sendPasswordResetEmail,
    isEmailResendAllowed,
} from "./utils.js";
import { ensureValidEmail } from "../../validation.js";
import type { EmailFromField } from '../../../email/core/types.js';
import { GetPasswordResetEmailContentFn } from './types.js';
import HttpError from 'wasp/core/HttpError'

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

        /**
         * By doing fake work, we make it harder to enumerate users by measuring
         * the time it takes to respond. If we would respond immediately, an attacker
         * could measure the time it takes to respond and figure out if the user exists.
         */

        if (!authIdentity) {
            await doFakeWork();
            return res.json({ success: true });
        }

        const providerData = deserializeAndSanitizeProviderData<'email'>(authIdentity.providerData);
        const { isResendAllowed, timeLeft } = isEmailResendAllowed(providerData, 'passwordResetSentAt');
        if (!isResendAllowed) {
            throw new HttpError(400, `Please wait ${timeLeft} secs before trying again.`);
        }

        const passwordResetLink = await createPasswordResetLink(args.email, clientRoute);
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
