import { Request, Response } from 'express';
import {
    findAuthWithUserBy,
    doFakeWork,
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
        const args = req.body || {};
        ensureValidEmail(args);

        args.email = args.email.toLowerCase();

        const auth = await findAuthWithUserBy({ email: args.email });
    
        // User not found or not verified - don't leak information
        if (!auth || !auth.isEmailVerified) {
            await doFakeWork();
            return res.json({ success: true });
        }

        if (!isEmailResendAllowed(auth, 'passwordResetSentAt')) {
            return res.status(400).json({ success: false, message: "Please wait a minute before trying again." });
        }
    
        const passwordResetLink = await createPasswordResetLink(auth, clientRoute);
        try {
            await sendPasswordResetEmail(
                auth.email,
                {
                    from: fromField,
                    to: auth.email,
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
