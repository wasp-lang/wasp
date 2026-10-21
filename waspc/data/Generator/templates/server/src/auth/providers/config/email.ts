{{={= =}=}}
import { createEmailAuthProvider } from "@wasp.sh/lib-auth/node";
import { env } from "wasp/server";
import {
  emailVerification,
  type AuthHookTypes,
  type GetPasswordResetEmailContentFn,
  type GetVerificationEmailContentFn,
} from "wasp/server/auth";
import { jwt } from "wasp/server/auth/jwt";
import { sessions } from "wasp/server/auth/session";
import { authIdentities } from "wasp/server/auth/utils";
import type { EmailFromField } from "wasp/server/email/core/types";
import * as hooks from "../../hooks.js";

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
    name: {=& fromField.name =},
    email: {=& fromField.email =},
};

const config = createEmailAuthProvider<AuthHookTypes>(
  {
    id: "{= providerId =}",
    displayName: "{= displayName =}",
    userSignupFields: _waspUserSignupFields,
    fromField,
    emailVerificationClientRoute: {=& emailVerificationClientRoute =},
    passwordResetClientRoute: {=& passwordResetClientRoute =},
    getVerificationEmailContent: _waspGetVerificationEmailContent,
    getPasswordResetEmailContent: _waspGetPasswordResetEmailContent,
    {=# isDevelopment =}
    isEmailAutoVerified: env.SKIP_EMAIL_VERIFICATION_IN_DEV,
    {=/ isDevelopment =}
    {=^ isDevelopment =}
    isEmailAutoVerified: false,
    {=/ isDevelopment =}
  },
  {
    identities: authIdentities,
    sessions,
    hooks,
    jwt,
    emailVerification,
  },
);

export default config;
