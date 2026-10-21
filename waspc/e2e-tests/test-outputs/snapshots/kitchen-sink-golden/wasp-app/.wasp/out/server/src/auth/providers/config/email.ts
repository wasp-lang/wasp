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

import { emailUserSignupFields } from "../../../../../../../src/features/auth/providers/email"
const _waspUserSignupFields = emailUserSignupFields

import { getVerificationEmailContent } from "../../../../../../../src/features/auth/providers/email"
const _waspGetVerificationEmailContent: GetVerificationEmailContentFn = getVerificationEmailContent;
import { getPasswordResetEmailContent } from "../../../../../../../src/features/auth/providers/email"
const _waspGetPasswordResetEmailContent: GetPasswordResetEmailContentFn = getPasswordResetEmailContent;


const fromField: EmailFromField = {
    name: "Wasp's \"Kitchen Sink\"",
    email: "kitchen-sink@wasp.sh",
};

const config = createEmailAuthProvider<AuthHookTypes>(
  {
    id: "email",
    displayName: "Email and password",
    userSignupFields: _waspUserSignupFields,
    fromField,
    emailVerificationClientRoute: "/email-verification-",
    passwordResetClientRoute: "/password-reset",
    getVerificationEmailContent: _waspGetVerificationEmailContent,
    getPasswordResetEmailContent: _waspGetPasswordResetEmailContent,
    isEmailAutoVerified: env.SKIP_EMAIL_VERIFICATION_IN_DEV,
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
