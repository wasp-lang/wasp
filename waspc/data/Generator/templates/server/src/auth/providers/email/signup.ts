import { Request, Response } from 'express';
import { EmailFromField } from "../../../email/core/types.js";
import {
    createUser,
    createProviderId,
    findAuthIdentity,
    deleteUserByAuthId,
    doFakeWork,
    deserializeAndSanitizeProviderData,
    sanitizeAndSerializeProviderData,
    rethrowPossibleAuthError,
} from "../../utils.js";
import {
    createEmailVerificationLinkWithToken,
    sendEmailVerificationEmail,
    isEmailResendAllowed,
} from "./utils.js";
import { ensureValidEmail, ensureValidPassword, ensurePasswordIsPresent } from "../../validation.js";
import { GetVerificationEmailContentFn } from './types.js';
import { validateAndGetAdditionalFields } from '../../utils.js'
import HttpError from '../../../core/HttpError.js';

export function getSignupRoute({
    fromField,
    clientRoute,
    getVerificationEmailContent,
    allowUnverifiedLogin,
}: {
    fromField: EmailFromField;
    clientRoute: string;
    getVerificationEmailContent: GetVerificationEmailContentFn;
    allowUnverifiedLogin: boolean;
}) {
    return async function signup(
        req: Request<{ email: string; password: string; }>,
        res: Response,
    ): Promise<Response<{ success: true }>> {
        const fields = req.body;
        ensureValidArgs(fields);
        
        const providerId = createProviderId("email", fields.email);
        const existingAuthIdentity = await findAuthIdentity(providerId);

        if (existingAuthIdentity && !allowUnverifiedLogin) {
            const providerData = deserializeAndSanitizeProviderData<'email'>(existingAuthIdentity.providerData);

            // 1a. User already exists and is verified - don't leak information
            if (providerData.isEmailVerified) {
                await doFakeWork();
                return res.json({ success: true });
            }

            const { isResendAllowed, timeLeft } = isEmailResendAllowed(providerData, 'passwordResetSentAt');
            if (!isResendAllowed) {
                throw new HttpError(400, `Please wait ${timeLeft} secs before trying again.`);
            }

            try {
                // 1b. User exists but is not verified - delete the user and create a new one
                await deleteUserByAuthId(existingAuthIdentity.authId);
            } catch (e: unknown) {
                rethrowPossibleAuthError(e);
            }

        } else if (existingAuthIdentity && allowUnverifiedLogin) {
            // 2. User already exists **and we allow unverified login**
            throw new HttpError(422, "User with that email already exists.")
        }

        const userFields = await validateAndGetAdditionalFields(fields);

        const newUserProviderData = await sanitizeAndSerializeProviderData<'email'>({
            hashedPassword: fields.password,
            isEmailVerified: false,
            emailVerificationSentAt: null,
            passwordResetSentAt: null,
        });

        try {
            await createUser(
                providerId,
                newUserProviderData,
                userFields,
            );
        } catch (e: unknown) {
            rethrowPossibleAuthError(e);
        }

        const verificationLink = await createEmailVerificationLinkWithToken(fields.email, clientRoute);
        try {
            await sendEmailVerificationEmail(
                fields.email,
                {
                    from: fromField,
                    to: fields.email,
                    ...getVerificationEmailContent({ verificationLink }),
                }
            );
        } catch (e: unknown) {
            console.error("Failed to send email verification email:", e);
            throw new HttpError(500, "Failed to send email verification email.");
        } 
      
        return res.json({ success: true });
    };
}

function ensureValidArgs(args: unknown): void {
    ensureValidEmail(args);
    ensurePasswordIsPresent(args);
    ensureValidPassword(args);
}
  
