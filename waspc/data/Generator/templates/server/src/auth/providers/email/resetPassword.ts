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

export async function resetPassword(
    req: Request<{ token: string; password: string; }>,
    res: Response,
): Promise<Response<{ success: true } | { success: false; message: string }>> {
    const args = req.body ?? {};
    ensureValidArgs(args);

    const { token, password } = args;
    try {
        const { id: email } = await verifyToken(token);

        const providerId = createProviderId('email', email);
        const authIdentity = await findAuthIdentity(providerId);
        if (!authIdentity) {
            return res.status(400).json({ success: false, message: 'Invalid token' });
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
        return res.status(400).json({ success: false, message: `Password reset failed, ${reason} token`});
    }
    res.json({ success: true });
};

function ensureValidArgs(args: unknown): void {
    ensureTokenIsPresent(args);
    ensurePasswordIsPresent(args);
    ensureValidPassword(args);
}
