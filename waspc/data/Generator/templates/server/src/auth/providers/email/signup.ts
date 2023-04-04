import { Request, Response } from 'express';
import { EmailSender, EmailFromField } from "../../../email/core/types.js";
import { handleRejection } from "../../../utils.js";
import { createEmailVerificationLink, createUser, findUserBy, deleteUser, doFakeWork, ensureValidEmailAndPassword } from "../../utils.js";
import { GetVerificationEmailContentFn } from './types.js';

export function getSignupRoute({
    emailSender,
    fromField,
    clientRoute,
    getVerificationEmailContent,
}: {
    emailSender: EmailSender;
    fromField: EmailFromField;
    clientRoute: string;
    getVerificationEmailContent: GetVerificationEmailContentFn;
}) {
    return handleRejection(async (req: Request<{ email: string; password: string; }>, res: Response) => {
        const userFields = req.body;
        ensureValidEmailAndPassword(userFields);
        
        userFields.email = userFields.email.toLowerCase();

        const existingUser  = await findUserBy<'email'>({ email: userFields.email });
        // User already exists and is verified - don't leak information
        if (existingUser && existingUser.isEmailVerified) {
            await doFakeWork();
            return res.json({ success: true });
        } else if (existingUser && !existingUser.isEmailVerified) {
            // TODO: Check using emailVerificationSentAt to ensure we send email once per minute
            await deleteUser(existingUser);
        }
    
        const user = await createUser(userFields);

        const verificationLink = await createEmailVerificationLink(user, clientRoute);
        try {
            await emailSender.send({
                from: fromField,
                to: userFields.email,
                ...getVerificationEmailContent({ verificationLink }),
            });
        } catch (e: any) {
            console.error("Failed to send email verification email:", e);
            return res.status(500).json({ success: false, message: "Failed to send email verification email." });
        } 
      
        return res.json({ success: true });
    });
}
