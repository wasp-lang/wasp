{{={= =}=}}
import { Router, Request, Response, NextFunction } from "express";

import { ProviderConfig } from "../types.js";
import { emailSender } from '../../../email/index.js';
import type { EmailFromField } from '../../../email/core/types.js';

import { getLoginRoute } from "../email/login.js";
import { getSignupRoute } from "../email/signup.js";
import { getRequestPasswordResetRoute } from "../email/requestPasswordReset.js";
import { resetPassword } from "../email/resetPassword.js";
import { verifyEmail } from "../email/verifyEmail.js";
import { GetVerificationEmailContentFn, GetPasswordResetEmailContentFn } from "../email/types.js";
import AuthError from "../../../core/AuthError.js";
import HttpError from "../../../core/HttpError.js";

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

        router.post('/login', handleAuthErrorMiddleware, getLoginRoute({
            allowUnverifiedLogin: {=# allowUnverifiedLogin =}true{=/ allowUnverifiedLogin =}{=^ allowUnverifiedLogin =}false{=/ allowUnverifiedLogin =},
        }));
        router.post('/signup', getSignupRoute({
            emailSender,
            fromField,
            clientRoute: '{= emailVerificationClientRoute =}',
            getVerificationEmailContent: _waspGetVerificationEmailContent,
        }));
        router.post('/request-password-reset', handleAuthErrorMiddleware, getRequestPasswordResetRoute({
            emailSender,
            fromField,
            clientRoute: '{= passwordResetClientRoute =}',
            getPasswordResetEmailContent: _waspGetPasswordResetEmailContent,
        }));
        router.post('/reset-password', handleAuthErrorMiddleware, resetPassword);
        router.post('/verify-email', handleAuthErrorMiddleware, verifyEmail);

        return router;
    },
}

function handleAuthErrorMiddleware(_req: Request, res: Response, next: NextFunction): void {
    try {
        next();
    } catch (e: unknown) {
        if (e instanceof AuthError) {
            throw new HttpError(422, 'Validation failed', { message: e.message })
        } else {
            throw e;
        }
    }    
}

export default config;
