import { Request, Response } from 'express';
import { findAuthWithUserBy, verifyToken } from "../../utils.js";
import { updateUserPassword } from "./utils.js";
import { ensureTokenIsPresent, ensurePasswordIsPresent, ensureValidPassword } from "../../validation.js";
import { tokenVerificationErrors } from "./types.js";

export async function resetPassword(
    req: Request<{ token: string; password: string; }>,
    res: Response,
): Promise<Response<{ success: true } | { success: false; message: string }>> {
    const args = req.body || {};
    ensureValidArgs(args);

    const { token, password } = args;
    try {
        const { id: userId } = await verifyToken(token);
        const user = await findAuthWithUserBy({ id: userId });
        if (!user) {
            return res.status(400).json({ success: false, message: 'Invalid token' });
        }
        await updateUserPassword(userId, password);
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
