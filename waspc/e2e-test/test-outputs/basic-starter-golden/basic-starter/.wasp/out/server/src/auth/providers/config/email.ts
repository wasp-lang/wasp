import { Router } from "express";

import { ProviderConfig } from "wasp/auth/providers/types";
import type { EmailFromField } from "wasp/server/email/core/types";

import { getLoginRoute } from "../email/login.js";
import { getSignupRoute } from "../email/signup.js";
import { getRequestPasswordResetRoute } from "../email/requestPasswordReset.js";
import { resetPassword } from "../email/resetPassword.js";
import { verifyEmail } from "../email/verifyEmail.js";
import { GetVerificationEmailContentFn, GetPasswordResetEmailContentFn } from "wasp/server/auth/email";
import { defineHandler } from "wasp/server/utils";
import { env } from "wasp/server";

import { userSignupFields } from '../../../../../../../src/auth/email/userSignupFields'
const _waspUserSignupFields = userSignupFields


const _waspGetVerificationEmailContent: GetVerificationEmailContentFn = ({ verificationLink }) => ({
    subject: 'Verify your email',
    text: `Click the link below to verify your email: ${verificationLink}`,
    html: `
        <p>Click the link below to verify your email</p>
        <a href="${verificationLink}">Verify email</a>
    `,
});
const _waspGetPasswordResetEmailContent: GetPasswordResetEmailContentFn = ({ passwordResetLink }) => ({
    subject: 'Reset your password',
    text: `Click the link below to reset your password: ${passwordResetLink}`,
    html: `
        <p>Click the link below to reset your password</p>
        <a href="${passwordResetLink}">Reset password</a>
    `,
});

const fromField: EmailFromField = {
    name: 'Basic App',
    email: 'hello@example.com',
};

const config: ProviderConfig = {
    id: "email",
    displayName: "Email and password",
    createRouter() {
        const router = Router();

        const loginRoute = defineHandler(getLoginRoute());
        router.post('/login', loginRoute);

        const signupRoute = defineHandler(getSignupRoute({
            userSignupFields: _waspUserSignupFields,
            fromField,
            clientRoute: '/email-verification',
            getVerificationEmailContent: _waspGetVerificationEmailContent,
            isEmailAutoVerified: env.SKIP_EMAIL_VERIFICATION_IN_DEV,
        }));
        router.post('/signup', signupRoute);

        const requestPasswordResetRoute = defineHandler(getRequestPasswordResetRoute({
            fromField,
            clientRoute: '/password-reset',
            getPasswordResetEmailContent: _waspGetPasswordResetEmailContent,
        }));
        router.post('/request-password-reset', requestPasswordResetRoute);

        router.post('/reset-password', defineHandler(resetPassword));
        router.post('/verify-email', defineHandler(verifyEmail));

        return router;
    },
}

export default config;
