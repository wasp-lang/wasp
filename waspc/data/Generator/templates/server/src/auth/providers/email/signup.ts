import { Request, Response } from 'express';
import { EmailFromField } from "../../../email/core/types.js";
import {
    createEmailVerificationLink,
    createUser,
    findUserBy,
    deleteUser,
    doFakeWork,
    ensureValidEmailAndPassword,
    sendEmailVerificationEmail,
    isEmailResendAllowed,
} from "../../utils.js";
import { GetVerificationEmailContentFn } from './types.js';

export function getSignupRoute({
    fromField,
    clientRoute,
    getVerificationEmailContent,
}: {
    fromField: EmailFromField;
    clientRoute: string;
    getVerificationEmailContent: GetVerificationEmailContentFn;
}) {
    return async function signup(
        req: Request<{ email: string; password: string; }>,
        res: Response,
    ): Promise<Response<{ success: true } | { success: false; message: string }>> {
        const userFields = req.body;
        ensureValidEmailAndPassword(userFields);
        
        userFields.email = userFields.email.toLowerCase();

        const existingUser  = await findUserBy<'email'>({ email: userFields.email.toLowerCase() });
        // User already exists and is verified - don't leak information
        if (existingUser && existingUser.isEmailVerified) {
            await doFakeWork();
            return res.json({ success: true });
        } else if (existingUser && !existingUser.isEmailVerified) {
            if (!isEmailResendAllowed(existingUser, 'emailVerificationSentAt')) {
                return res.status(400).json({ success: false, message: "Please wait a minute before trying again." });
            }
            await deleteUser(existingUser);
        }
    
        const user = await createUser(userFields);

        const verificationLink = await createEmailVerificationLink(user, clientRoute);
        try {
            await sendEmailVerificationEmail(
                userFields.email,
                {
                    from: fromField,
                    to: userFields.email,
                    ...getVerificationEmailContent({ verificationLink }),
                }
            );
        } catch (e: any) {
            console.error("Failed to send email verification email:", e);
            return res.status(500).json({ success: false, message: "Failed to send email verification email." });
        } 
      
        return res.json({ success: true });
    };
}
