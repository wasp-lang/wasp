{{={= =}=}}
import { createAuthForms } from "@wasp.sh/lib-auth/browser";
import { config } from "../../client/index.js";
{=# isEmailAuthEnabled =}
import * as emailActions from "../email/actions.js";
{=/ isEmailAuthEnabled =}
{=# isUsernameAndPasswordAuthEnabled =}
import * as usernameAndPasswordActions from "../username/actions.js";
{=/ isUsernameAndPasswordAuthEnabled =}

// PUBLIC API
export const {
  LoginForm,
  SignupForm,
  ForgotPasswordForm,
  ResetPasswordForm,
  VerifyEmailForm,
} = createAuthForms({
  onAuthSucceededRedirectTo: {=& onAuthSucceededRedirectTo =},
  {=# isEmailAuthEnabled =}
  email: emailActions,
  {=/ isEmailAuthEnabled =}
  {=# isUsernameAndPasswordAuthEnabled =}
  usernameAndPassword: usernameAndPasswordActions,
  {=/ isUsernameAndPasswordAuthEnabled =}
  socialProviders: [
    {=# socialProviders =}
    { name: "{= providerName =}", signInUrl: `${config.apiUrl}{= signInPath =}` },
    {=/ socialProviders =}
  ],
  {=# isDummyEmailSenderNoticeEnabled =}
  isDummyEmailSenderNoticeEnabled: true,
  {=/ isDummyEmailSenderNoticeEnabled =}
  {=^ isDummyEmailSenderNoticeEnabled =}
  isDummyEmailSenderNoticeEnabled: false,
  {=/ isDummyEmailSenderNoticeEnabled =}
});
