import { Request, Response } from 'express';
import {
    verifyToken,
    findAuthIdentity,
    updateAuthIdentityProviderData,
    deserializeAndSanitizeProviderData,
} from '../../utils.js';
import { tokenVerificationErrors } from './types.js';

export async function verifyEmail(
    req: Request<{ token: string }>,
    res: Response,
): Promise<Response<{ success: true } | { success: false, message: string }>> {
    try {
        const { token } = req.body;
        const { id: providerUserId } = await verifyToken(token);

        const authIdentity = await findAuthIdentity('email', providerUserId);
        const providerData = deserializeAndSanitizeProviderData<'email'>(authIdentity.providerData);
        await updateAuthIdentityProviderData('email', providerUserId, providerData, {
            isEmailVerified: true,
        });
    } catch (e) {
        const reason = e.name === tokenVerificationErrors.TokenExpiredError
            ? 'expired'
            : 'invalid';
        return res.status(400).json({ success: false, message: `Token is ${reason}` });
    }

    return res.json({ success: true });
};

