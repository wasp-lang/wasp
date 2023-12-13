import { Request, Response } from 'express';
import {
    findAuthIdentity,
    doFakeWork,
    deserializeProviderData,
} from "../../utils.js";
import {
    createPasswordResetLink,
    sendPasswordResetEmail,
    isEmailResendAllowed,
} from "./utils.js";
import { ensureValidEmail } from "../../validation.js";
import type { EmailFromField } from '../../../email/core/types.js';
import { GetPasswordResetEmailContentFn } from './types.js';

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
    ): Promise<Response<{ success: true } | { success: false; message: string }>> {
        const args = req.body ?? {};
        ensureValidEmail(args);

        const authIdentity = await findAuthIdentity("email", args.email);

        // User not found or not verified - don't leak information
        if (!authIdentity) {
            await doFakeWork();
            return res.json({ success: true });
        }

        const providerData = deserializeProviderData<'email'>(authIdentity.providerData);
        if (!providerData.isEmailVerified) {
            await doFakeWork();
            return res.json({ success: true });
        }

        if (!isEmailResendAllowed(providerData, 'passwordResetSentAt')) {
            return res.status(400).json({ success: false, message: "Please wait a minute before trying again." });
        }

        const passwordResetLink = await createPasswordResetLink({
            id: authIdentity.authId,
        }, clientRoute);
        try {
            const email = authIdentity.providerUserId
            await sendPasswordResetEmail(
                email,
                {
                    from: fromField,
                    to: email,
                    ...getPasswordResetEmailContent({ passwordResetLink }),
                }
            );
        } catch (e: any) {
            console.error("Failed to send password reset email:", e);
            return res.status(500).json({ success: false, message: "Failed to send password reset email." });
        }
    
        res.json({ success: true });
    };
}
