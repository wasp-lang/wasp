{{={= =}=}}
import { Router } from "express";

import { ProviderConfig } from "../types.js";
import { emailSender } from '../../../email/index.js';
import type { EmailFromField } from '../../../email/core/types.js';

import { login } from "../email/login.js";
import { getSignupRoute } from "../email/signup.js";
import { getRequestPasswordResetRoute } from "../email/requestPasswordReset.js";
import { resetPassword } from "../email/resetPassword.js";
import { getVerifyEmailRoute } from "../email/verifyEmail.js";
import { GetVerificationEmailContentFn, GetPasswordResetEmailContentFn } from "../email/types.js";

{=# getVerificationEmailContent.isDefined =}
{=& getVerificationEmailContent.importStatement =}
const _waspGetVerificationEmailContent: GetVerificationEmailContentFn = {= getVerificationEmailContent.importIdentifier =};
{=/ getVerificationEmailContent.isDefined =}
{=# getPasswordResetEmailContent.isDefined =}
{=& getPasswordResetEmailContent.importStatement =}
const _waspGetPasswordResetEmailContent: GetPasswordResetEmailContentFn = {= getPasswordResetEmailContent.importIdentifier =};
{=/ getPasswordResetEmailContent.isDefined =}

{=^ getVerificationEmailContent.isDefined =}
const _waspGetVerificationEmailContent: GetVerificationEmailContentFn = ({ verificationLink }) => ({
    subject: 'Verify your email',
    text: `Click the link below to verify your email: ${verificationLink}`,
    html: `
        <p>Click the link below to verify your email</p>
        <a href="${verificationLink}">Verify email</a>
    `,
});
{=/ getVerificationEmailContent.isDefined =}
{=^ getPasswordResetEmailContent.isDefined =}
const _waspGetPasswordResetEmailContent: GetPasswordResetEmailContentFn = ({ passwordResetLink }) => ({
    subject: 'Reset your password',
    text: `Click the link below to reset your password: ${passwordResetLink}`,
    html: `
        <p>Click the link below to reset your password</p>
        <a href="${passwordResetLink}">Reset password</a>
    `,
});
{=/ getPasswordResetEmailContent.isDefined =}

const fromField: EmailFromField = {
    name: '{= fromField.name =}',
    email: '{= fromField.email =}',
};

const config: ProviderConfig = {
    id: "{= providerId =}",
    displayName: "{= displayName =}",
    createRouter() {
        const router = Router();

        router.post('/login', login);
        router.post('/signup', getSignupRoute({
            emailSender,
            fromField,
            getVerificationEmailContent: _waspGetVerificationEmailContent,
        }));
        router.post('/request-password-reset', getRequestPasswordResetRoute({
            emailSender,
            fromField,
            clientRoute: '{= passwordResetClientRoute =}',
            getPasswordResetEmailContent: _waspGetPasswordResetEmailContent,
        }));
        router.post('/reset-password', resetPassword);
        router.get('/verify-email', getVerifyEmailRoute({
            onVerifySuccessRedirectTo: '{= onVerifySuccessRedirectTo =}',
        }));

        return router;
    },
}

export default config;
