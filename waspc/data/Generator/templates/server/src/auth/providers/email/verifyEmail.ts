import waspServerConfig from '../../../config.js';

import { handleRejection } from "../../../utils.js";
import { updateUserEmailVerification, verifyToken } from '../../utils.js';

export function getVerifyEmailRoute({
    onVerifySuccessRedirectTo,
}: {
    onVerifySuccessRedirectTo: string;
}) {
    return handleRejection(async (req, res) => {
        const args = req.query || {};
        const { token } = args;
        try {
            const { id: userId } = await verifyToken(token);
            await updateUserEmailVerification(userId);
        } catch (e) {
            console.warn('Email verification failed', e);
            console.warn('Token:', token);
            return res.redirect(waspServerConfig.frontendUrl);
        }
    
        const redirectUrl = `${waspServerConfig.frontendUrl}${onVerifySuccessRedirectTo}?result=success`;
        res.redirect(redirectUrl);
    });
}
