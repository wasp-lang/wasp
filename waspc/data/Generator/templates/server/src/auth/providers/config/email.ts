{{={= =}=}}
import { Router, Request, Response, NextFunction } from "express";

import { ProviderConfig } from "../types.js";
import type { EmailFromField } from '../../../email/core/types.js';

import { getLoginRoute } from "../email/login.js";
import { getSignupRoute } from "../email/signup.js";
import { getRequestPasswordResetRoute } from "../email/requestPasswordReset.js";
import { resetPassword } from "../email/resetPassword.js";
import { verifyEmail } from "../email/verifyEmail.js";
import { GetVerificationEmailContentFn, GetPasswordResetEmailContentFn } from "../email/types.js";
import { handleRejection } from "../../../utils.js";

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

        const loginRoute = handleRejection(getLoginRoute({
            allowUnverifiedLogin: {=# allowUnverifiedLogin =}true{=/ allowUnverifiedLogin =}{=^ allowUnverifiedLogin =}false{=/ allowUnverifiedLogin =},
        }));
        router.post('/login', loginRoute);

        const signupRoute = handleRejection(getSignupRoute({
            fromField,
            clientRoute: '{= emailVerificationClientRoute =}',
            getVerificationEmailContent: _waspGetVerificationEmailContent,
        }));
        router.post('/signup', signupRoute);
        
        const requestPasswordResetRoute = handleRejection(getRequestPasswordResetRoute({
            fromField,
            clientRoute: '{= passwordResetClientRoute =}',
            getPasswordResetEmailContent: _waspGetPasswordResetEmailContent,
        }));
        router.post('/request-password-reset', requestPasswordResetRoute);

        router.post('/reset-password', handleRejection(resetPassword));
        router.post('/verify-email', handleRejection(verifyEmail));

        return router;
    },
}

export default config;
