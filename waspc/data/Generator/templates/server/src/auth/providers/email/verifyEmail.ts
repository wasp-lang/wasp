import { Request, Response } from 'express';
import { updateAuthEmailVerification } from './utils.js';
import { verifyToken } from '../../utils.js';
import { tokenVerificationErrors } from './types.js';

export async function verifyEmail(
    req: Request<{ token: string }>,
    res: Response,
): Promise<Response<{ success: true } | { success: false, message: string }>> {
    try {
        const { token } = req.body;
        const { id: authId } = await verifyToken(token);
        await updateAuthEmailVerification(authId);
    } catch (e) {
        const reason = e.name === tokenVerificationErrors.TokenExpiredError
            ? 'expired'
            : 'invalid';
        return res.status(400).json({ success: false, message: `Token is ${reason}` });
    }

    return res.json({ success: true });
};

