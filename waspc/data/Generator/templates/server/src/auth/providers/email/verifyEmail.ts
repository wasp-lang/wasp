import waspServerConfig from '../../../config.js';

import { handleRejection } from "../../../utils.js";
import { updateUserEmailVerification, verifyToken } from '../../utils.js';
import { tokenVerificationErrors } from './types.js';

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
            const reason = e.name === tokenVerificationErrors.TokenExpiredError
                ? 'expired'
                : 'invalid';
            // TODO: implement on error redirect route
            const errorRedirectUrl = `${waspServerConfig.frontendUrl}/?emailVerificationStatus=${reason}`;
            return res.redirect(errorRedirectUrl);
        }
    
        const redirectUrl = `${waspServerConfig.frontendUrl}${onVerifySuccessRedirectTo}?emailVerificationStatus=success`;
        res.redirect(redirectUrl);
    });
}
