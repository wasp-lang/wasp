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
    createEmailVerificationLink,
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

        /**
         * 
         * There are two cases to consider in the case of an existing user:
         * - if we allow unverified login
         * - if the user is already verified
         * 
         * Let's see what happens when we **don't** allow unverified login:
         * 
         * We are handling the case of an existing auth identity in two ways:
         * 
         * 1. If the user already exists and is verified, we don't want
         *   to leak that piece of info and instead we pretend that the user
         *   was created successfully.
         *    - This prevents the attacker from learning which emails already have
         *        an account created.
         * 
         * 2. If the user is not verified:
         *   - We check when we last sent a verification email and if it was less than X seconds ago,
         *     we don't send another one.
         *   - If it was more than X seconds ago, we delete the user and create a new one.
         *   - This prevents the attacker from creating an account with somebody
         *     else's email address and therefore permanently making that email
         *     address unavailable for later account creation (by real owner).
         */
        if (existingAuthIdentity) {
            if (allowUnverifiedLogin) {
                /**
                 * This is the case where we allow unverified login.
                 * 
                 * If we pretended that the user was created successfully that would bring
                 * us little value: the attacker would not be able to login and figure out
                 * if the user exists or not, anyway.
                 * 
                 * So, we throw an error that says that the user already exists.
                 */
                throw new HttpError(422, "User with that email already exists.")
            }
            
            const providerData = deserializeAndSanitizeProviderData<'email'>(existingAuthIdentity.providerData);

            // TOOD: faking work makes sense if the time spent on faking the work matches the time
            // it would take to send the email. Atm, the fake work takes obviously longer than sending
            // the email!
            if (providerData.isEmailVerified) {
                await doFakeWork();
                return res.json({ success: true });
            }
            
            // TODO: we are still leaking information here since when we are faking work
            // we are not checking if the email was sent or not!
            const { isResendAllowed, timeLeft } = isEmailResendAllowed(providerData, 'passwordResetSentAt');
            if (!isResendAllowed) {
                throw new HttpError(400, `Please wait ${timeLeft} secs before trying again.`);
            }

            try {
                await deleteUserByAuthId(existingAuthIdentity.authId);
            } catch (e: unknown) {
                rethrowPossibleAuthError(e);
            }
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
                // Using any here because we want to avoid TypeScript errors and
                // rely on Prisma to validate the data.
                userFields as any
            );
        } catch (e: unknown) {
            rethrowPossibleAuthError(e);
        }

        const verificationLink = await createEmailVerificationLink(fields.email, clientRoute);
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
  
