import { Request, Response } from 'express';
import { handleRejection } from "../../../utils.js";
import { createPasswordResetLink, findUserBy, doFakeWork } from "../../utils.js";
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
    return handleRejection(async (req: Request<{ username: string; }>, res: Response) => {
        const args = req.body || {};
        const user = await findUserBy<'username'>({ username: args.email });
    
        // User not found or not verified - don't leak information
        if (!user || !user.isEmailVerified) {
            await doFakeWork();
            return res.json({ success: true });
        }
    
        const passwordResetLink = await createPasswordResetLink(user, clientRoute);
        try {
            await emailSender.send({
                from: fromField,
                to: user.username,
                ...getPasswordResetEmailContent({ passwordResetLink }),
            });
        } catch (e: any) {
            console.error("Failed to send password reset email:", e);
            return res.status(500).json({ success: false, message: "Failed to send password reset email." });
        }
    
        res.json({ success: true });
    });
}
