import { Request, Response } from 'express';
import { handleRejection } from "../../../utils.js";
import { updateUserEmailVerification, verifyToken } from '../../utils.js';
import { tokenVerificationErrors } from './types.js';

export const verifyEmail = handleRejection(async (req: Request<{ token: string }>, res: Response) => {
    try {
        const { token } = req.body;
        const { id: userId } = await verifyToken(token);
        await updateUserEmailVerification(userId);
    } catch (e) {
        const reason = e.name === tokenVerificationErrors.TokenExpiredError
            ? 'expired'
            : 'invalid';
        return res.status(400).json({ success: false, reason });
    }

    return res.json({ success: true });
});

