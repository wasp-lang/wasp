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

import { userSignupFields } from '../../../../../../../src/features/auth/providers/email'
const _waspUserSignupFields = userSignupFields

import { getVerificationEmailContent } from '../../../../../../../src/features/auth/providers/email'
const _waspGetVerificationEmailContent: GetVerificationEmailContentFn = getVerificationEmailContent;
import { getPasswordResetEmailContent } from '../../../../../../../src/features/auth/providers/email'
const _waspGetPasswordResetEmailContent: GetPasswordResetEmailContentFn = getPasswordResetEmailContent;


const fromField: EmailFromField = {
    name: 'Wasp Kitchen Sink',
    email: 'kitchen-sink@wasp.sh',
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
            clientRoute: '/email-verification-',
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
