import { Request, Response } from 'express';
import {
    createProviderId,
    findAuthIdentity,
    updateAuthIdentityProviderData,
    verifyToken,
    deserializeAndSanitizeProviderData,
} from "../../utils.js";
import { ensureTokenIsPresent, ensurePasswordIsPresent, ensureValidPassword } from "../../validation.js";
import { tokenVerificationErrors } from "./types.js";
import HttpError from '../../../core/HttpError.js';

export async function resetPassword(
    req: Request<{ token: string; password: string; }>,
    res: Response,
): Promise<Response<{ success: true }>> {
    const args = req.body ?? {};
    ensureValidArgs(args);

    const { token, password } = args;
    try {
        const { email } = await verifyToken<{ email: string }>(token);

        const providerId = createProviderId('email', email);
        const authIdentity = await findAuthIdentity(providerId);
        if (!authIdentity) {
            throw new HttpError(400, "Password reset failed, invalid token");
        }
        
        const providerData = deserializeAndSanitizeProviderData<'email'>(authIdentity.providerData);

        await updateAuthIdentityProviderData(providerId, providerData, {
            // The act of resetting the password verifies the email
            isEmailVerified: true,
            // The password will be hashed when saving the providerData
            // in the DB
            hashedPassword: password,
        });
    } catch (e) {
        const reason = e.name === tokenVerificationErrors.TokenExpiredError
            ? 'expired'
            : 'invalid';
        throw new HttpError(400, `Password reset failed, ${reason} token`);
    }
    return res.json({ success: true });
};

function ensureValidArgs(args: unknown): void {
    ensureTokenIsPresent(args);
    ensurePasswordIsPresent(args);
    ensureValidPassword(args);
}
