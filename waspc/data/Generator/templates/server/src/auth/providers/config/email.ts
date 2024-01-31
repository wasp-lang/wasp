{{={= =}=}}
import { Router, Request, Response, NextFunction } from "express";

import { ProviderConfig } from "wasp/auth/providers/types";
import type { EmailFromField } from "wasp/server/email/core/types";

import { getLoginRoute } from "../email/login.js";
import { getSignupRoute } from "../email/signup.js";
import { getRequestPasswordResetRoute } from "../email/requestPasswordReset.js";
import { resetPassword } from "../email/resetPassword.js";
import { verifyEmail } from "../email/verifyEmail.js";
import { GetVerificationEmailContentFn, GetPasswordResetEmailContentFn } from "wasp/server/auth/email";
import { handleRejection } from "wasp/server/utils";

{=# userSignupFields.isDefined =}
{=& userSignupFields.importStatement =}
const _waspUserSignupFields = {= userSignupFields.importIdentifier =}
{=/ userSignupFields.isDefined =}
{=^ userSignupFields.isDefined =}
const _waspUserSignupFields = undefined
{=/ userSignupFields.isDefined =}

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

        const loginRoute = handleRejection(getLoginRoute());
        router.post('/login', loginRoute);

        const signupRoute = handleRejection(getSignupRoute({
            userSignupFields: _waspUserSignupFields,
            fromField,
            clientRoute: '{= emailVerificationClientRoute =}',
            getVerificationEmailContent: _waspGetVerificationEmailContent,
            {=# isDevelopment =}
            isEmailAutoVerified: process.env.SKIP_EMAIL_VERIFICATION_IN_DEV === 'true',
            {=/ isDevelopment =}
            {=^ isDevelopment =}
            isEmailAutoVerified: false,
            {=/ isDevelopment =}
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
