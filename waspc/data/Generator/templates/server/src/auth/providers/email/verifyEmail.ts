import { Request, Response } from 'express';
import {
    verifyToken,
    createProviderId,
    findAuthIdentity,
    updateAuthIdentityProviderData,
    deserializeAndSanitizeProviderData,
} from '../../utils.js';
import { tokenVerificationErrors } from './types.js';
import HttpError from '../../../core/HttpError.js';


export async function verifyEmail(
    req: Request<{ token: string }>,
    res: Response,
): Promise<Response<{ success: true }> | void> {
    try {
        const { token } = req.body;
        const { email, token: verificationToken } = await verifyToken<{ email: string, token: string }>(token);

        const providerId = createProviderId('email', email);
        const authIdentity = await findAuthIdentity(providerId);
        const providerData = deserializeAndSanitizeProviderData<'email'>(authIdentity.providerData);

        if (verificationToken !== providerData.emailVerificationToken) {
            throw new HttpError(400, "Invalid token");
        }

        await updateAuthIdentityProviderData(providerId, providerData, {
            isEmailVerified: true,
        });
    } catch (e) {
        const reason = e.name === tokenVerificationErrors.TokenExpiredError
            ? 'expired'
            : 'invalid';
        throw new HttpError(400, `Token is ${reason}`);
    }

    return res.json({ success: true });
};

