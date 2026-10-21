import type * as AuthProviders from "@wasp.sh/lib-auth/browser";
import { createEmailAuthActions } from "@wasp.sh/lib-auth/browser";
import { api } from "../../api/index.js";
import type { UserEmailSignupFields } from "../providers";

type ExtraSignupFields = UserEmailSignupFields;

// PRIVATE API
export type EmailSignupData = AuthProviders.EmailSignupData<ExtraSignupFields>;

// PUBLIC API
export const { login, signup, requestPasswordReset, resetPassword, verifyEmail } =
  createEmailAuthActions<ExtraSignupFields>({
    api,
    paths: {
      login: "/auth/email/login",
      signup: "/auth/email/signup",
      requestPasswordReset: "/auth/email/request-password-reset",
      resetPassword: "/auth/email/reset-password",
      verifyEmail: "/auth/email/verify-email",
    },
  });
