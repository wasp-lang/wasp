import { Request, Response } from 'express';
import { EmailFromField } from "../../../email/core/types.js";
import {
    createAuthWithUser,
    findAuthWithUserBy,
    deleteAuth,
    doFakeWork,
} from "../../utils.js";
import {
    createEmailVerificationLink,
    sendEmailVerificationEmail,
    isEmailResendAllowed,
} from "./utils.js";
import { ensureValidEmail, ensureValidPassword, ensurePasswordIsPresent } from "../../validation.js";
import { GetVerificationEmailContentFn } from './types.js';
import { validateAndGetAdditionalFields } from '../../utils.js'

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
        ensureValidArgs(userFields);
        
        userFields.email = userFields.email.toLowerCase();

        const existingAuth  = await findAuthWithUserBy({ email: userFields.email });
        // User already exists and is verified - don't leak information
        if (existingAuth && existingAuth.isEmailVerified) {
            await doFakeWork();
            return res.json({ success: true });
        } else if (existingAuth && !existingAuth.isEmailVerified) {
            if (!isEmailResendAllowed(existingAuth, 'emailVerificationSentAt')) {
                return res.status(400).json({ success: false, message: "Please wait a minute before trying again." });
            }
            await deleteAuth(existingAuth);
        }

        const additionalFields = await validateAndGetAdditionalFields(userFields);
    
        const auth = await createAuthWithUser(
            {
                email: userFields.email,
                password: userFields.password,
            },
            additionalFields,
        );

        const verificationLink = await createEmailVerificationLink(auth, clientRoute);
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

function ensureValidArgs(args: unknown): void {
    ensureValidEmail(args);
    ensurePasswordIsPresent(args);
    ensureValidPassword(args);
}
  
