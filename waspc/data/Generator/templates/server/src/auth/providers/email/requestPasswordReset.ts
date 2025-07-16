import { Request, Response } from 'express';
import {
    createProviderId,
    findAuthIdentity,
    doFakeWork,
    getProviderDataWithPassword,
} from 'wasp/auth/utils';
import {
    createPasswordResetLink,
    sendPasswordResetEmail,
    isEmailResendAllowed,
} from "wasp/server/auth/email/utils";
import { ensureValidEmail } from 'wasp/auth/validation';
import type { EmailFromField } from 'wasp/server/email/core/types';
import { GetPasswordResetEmailContentFn } from 'wasp/server/auth/email';
import { HttpError } from 'wasp/server';

export function getRequestPasswordResetRoute({
   fromField,
   clientRoute,
   getPasswordResetEmailContent,
}: {
    fromField: EmailFromField;
    clientRoute: string;
    getPasswordResetEmailContent: GetPasswordResetEmailContentFn;
}) {
    return async function requestPasswordReset(
        req: Request<{ email: string; }>,
        res: Response,
    ): Promise<void> {
        const args = req.body ?? {};
        ensureValidEmail(args);

        const authIdentity = await findAuthIdentity(
            createProviderId("email", args.email),
        );

        /**
         * By doing fake work, we make it harder to enumerate users by measuring
         * the time it takes to respond. If we would respond immediately, an attacker
         * could measure the time it takes to respond and figure out if the user exists.
         */

        if (!authIdentity) {
            await doFakeWork();
            res.json({ success: true });
            return
        }

        const providerData = getProviderDataWithPassword<'email'>(authIdentity.providerData);
        const { isResendAllowed, timeLeft } = isEmailResendAllowed(providerData, 'passwordResetSentAt');
        if (!isResendAllowed) {
            throw new HttpError(400, `Please wait ${timeLeft} secs before trying again.`);
        }

        const passwordResetLink = await createPasswordResetLink(args.email, clientRoute);
        try {
            const email = authIdentity.providerUserId
            await sendPasswordResetEmail(
                email,
                {
                    from: fromField,
                    to: email,
                    ...getPasswordResetEmailContent({ passwordResetLink }),
                },
            );
        } catch (e: any) {
            console.error("Failed to send password reset email:", e);
            throw new HttpError(500, "Failed to send password reset email.");
        }
    
        res.json({ success: true });
    };
}
