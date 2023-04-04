import { Request, Response } from 'express';
import { createPasswordResetLink, findUserBy, doFakeWork, ensureValidEmail } from "../../utils.js";
import type { EmailSender, EmailFromField } from '../../../email/core/types.js';
import { GetPasswordResetEmailContentFn } from './types.js';

export function getRequestPasswordResetRoute({
   emailSender,
   fromField,
   clientRoute,
   getPasswordResetEmailContent,
}: {
    emailSender: EmailSender;
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
    
        const passwordResetLink = await createPasswordResetLink(user, clientRoute);
        try {
            await emailSender.send({
                from: fromField,
                to: user.email,
                ...getPasswordResetEmailContent({ passwordResetLink }),
            });
        } catch (e: any) {
            console.error("Failed to send password reset email:", e);
            return res.status(500).json({ success: false, message: "Failed to send password reset email." });
        }
    
        res.json({ success: true });
    };
}
