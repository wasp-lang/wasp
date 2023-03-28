import waspServerConfig from '../../../config.js';
import { EmailSender, EmailFromField } from "../../../email/core/types.js";
import { handleRejection } from "../../../utils.js";
import { createEmailVerificationToken, createUser } from "../../utils.js";
import { GetVerificationEmailContentFn } from './types.js';

export function getSignupRoute({
    emailSender,
    fromField,
    getVerificationEmailContent,
}: {
    emailSender: EmailSender;
    fromField: EmailFromField;
    getVerificationEmailContent: GetVerificationEmailContentFn;
}) {
    return handleRejection(async (req, res) => {
        const userFields = req.body || {};
    
        const user = await createUser(userFields);
    
        const verifyEmailToken = await createEmailVerificationToken(user);
        const verificationLink = `${waspServerConfig.backendUrl}/auth/email/verify-email?token=${verifyEmailToken}`;
        emailSender.send({
            from: fromField,
            to: req.body.email,
            ...getVerificationEmailContent({ verificationLink }),
        });
      
        return res.json({ success: true });
    });
}
