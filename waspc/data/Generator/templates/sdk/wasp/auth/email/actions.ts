{{={= =}=}}
import type * as AuthProviders from "@wasp.sh/lib-auth/browser";
import { createEmailAuthActions } from "@wasp.sh/lib-auth/browser";
import { api } from "../../api/index.js";
{=# isEmailUserSignupFieldsDefined =}
import type { UserEmailSignupFields } from "../providers";
{=/ isEmailUserSignupFieldsDefined =}

{=# isEmailUserSignupFieldsDefined =}
type ExtraSignupFields = UserEmailSignupFields;
{=/ isEmailUserSignupFieldsDefined =}
{=^ isEmailUserSignupFieldsDefined =}
type ExtraSignupFields = {};
{=/ isEmailUserSignupFieldsDefined =}

// PRIVATE API
export type EmailSignupData = AuthProviders.EmailSignupData<ExtraSignupFields>;

// PUBLIC API
export const { login, signup, requestPasswordReset, resetPassword, verifyEmail } =
  createEmailAuthActions<ExtraSignupFields>({
    api,
    paths: {
      login: "{= loginPath =}",
      signup: "{= signupPath =}",
      requestPasswordReset: "{= requestPasswordResetPath =}",
      resetPassword: "{= resetPasswordPath =}",
      verifyEmail: "{= verifyEmailPath =}",
    },
  });
