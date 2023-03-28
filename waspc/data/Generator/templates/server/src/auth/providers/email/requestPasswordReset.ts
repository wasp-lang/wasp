import waspServerConfig from '../../../config.js';
import { handleRejection } from "../../../utils.js";
import { createPasswordResetToken, findUserBy } from "../../utils.js";
import type { EmailSender, EmailFromField } from '../../../email/core/types.js';
import { GetPasswordResetEmailContentFn } from './types.js';

export function getRequestPasswordResetRoute({
   emailSender,
   fromField,
   clientRoute,
   getPasswordResetEmailContent,
}: {
    emailSender: EmailSender;
    fromField: EmailFromField;
    clientRoute: string;
    getPasswordResetEmailContent: GetPasswordResetEmailContentFn;
}) {
    return handleRejection(async (req, res) => {
        const args = req.body || {};
        const user = await findUserBy<'email'>({ email: args.email });
    
        if (!user) {
            return res.status(400).json({ error: 'User not found' });
        }
    
        const token = await createPasswordResetToken(user);
        const passwordResetLink = `${waspServerConfig.frontendUrl}${clientRoute}?token=${token}`;
        emailSender.send({
            from: fromField,
            to: user.email,
            ...getPasswordResetEmailContent({ passwordResetLink }),
        });
    
        res.json({ success: true });
    });
}
