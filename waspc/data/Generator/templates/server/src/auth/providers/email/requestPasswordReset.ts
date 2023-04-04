import { Request, Response } from 'express';
import {
    createPasswordResetLink,
    findUserBy,
    doFakeWork,
    ensureValidEmail,
    sendPasswordResetEmail,
    isEmailResendAllowed,
} from "../../utils.js";
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
        const user = await findUserBy<'email'>({ email: args.email });
    
        // User not found or not verified - don't leak information
        if (!user || !user.isEmailVerified) {
            await doFakeWork();
            return res.json({ success: true });
        }

        if (!isEmailResendAllowed(user, 'passwordResetSentAt')) {
            return res.status(400).json({ success: false, message: "Please wait a minute before trying again." });
        }
    
        const passwordResetLink = await createPasswordResetLink(user, clientRoute);
        try {
            await sendPasswordResetEmail(
                user.email,
                {
                    from: fromField,
                    to: user.email,
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
