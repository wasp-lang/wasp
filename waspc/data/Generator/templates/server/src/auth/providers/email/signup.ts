import { Request, Response } from 'express';
import { EmailFromField } from "../../../email/core/types.js";
import {
    createAuthWithUser,
    findAuthIdentity,
    deleteUserByAuthId,
    doFakeWork,
    deserializeProviderData,
    serializeProviderData,
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
        const fields = req.body;
        ensureValidArgs(fields);
        
        fields.email = fields.email.toLowerCase();

        const existingAuthIdentity = await findAuthIdentity("email", fields.email);
        // User already exists and is verified - don't leak information

        // TODO: check if the email is verified from providerData
        if (existingAuthIdentity) {
            const providerData = deserializeProviderData<'email'>(existingAuthIdentity.providerData);
            if (providerData.isEmailVerified) {
                await doFakeWork();
                return res.json({ success: true });
            } else if (!providerData.isEmailVerified) {
                if (!isEmailResendAllowed(providerData, 'emailVerificationSentAt')) {
                    return res.status(400).json({ success: false, message: "Please wait a minute before trying again." });
                }
                // TODO: verify this is correct
                await deleteUserByAuthId(existingAuthIdentity.authId);
            }
        }

        const additionalFields = await validateAndGetAdditionalFields(fields);

        const newUserProviderData = await serializeProviderData<'email'>({
            password: fields.password,
            isEmailVerified: false,
            emailVerificationSentAt: null,
            passwordResetSentAt: null,
        });
        const auth = await createAuthWithUser(
            {
                identities: {
                    create: {
                        providerName: "email",
                        providerUserId: fields.email,
                        providerData: newUserProviderData,
                    },
                }
            },
            additionalFields,
        );

        const verificationLink = await createEmailVerificationLink(auth, clientRoute);
        try {
            await sendEmailVerificationEmail(
                fields.email,
                {
                    from: fromField,
                    to: fields.email,
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
  
