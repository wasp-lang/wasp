import { createAuthForms } from "@wasp.sh/lib-auth/browser";
import { config } from "../../client/index.js";
import * as emailActions from "../email/actions.js";

// PUBLIC API
export const {
  LoginForm,
  SignupForm,
  ForgotPasswordForm,
  ResetPasswordForm,
  VerifyEmailForm,
} = createAuthForms({
  onAuthSucceededRedirectTo: "/",
  email: emailActions,
  socialProviders: [
    { name: "slack", signInUrl: `${config.apiUrl}/auth/slack/login` },
    { name: "discord", signInUrl: `${config.apiUrl}/auth/discord/login` },
    { name: "google", signInUrl: `${config.apiUrl}/auth/google/login` },
    { name: "github", signInUrl: `${config.apiUrl}/auth/github/login` },
    { name: "microsoft", signInUrl: `${config.apiUrl}/auth/microsoft/login` },
  ],
  isDummyEmailSenderNoticeEnabled: false,
});
