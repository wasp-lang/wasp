import { page, route, type Spec } from "@wasp.sh/spec";

import { EmailVerificationPage } from "./email/EmailVerificationPage" with { type: "ref" };
import { LoginPage } from "./email/LoginPage" with { type: "ref" };
import { PasswordResetPage } from "./email/PasswordResetPage" with { type: "ref" };
import { RequestPasswordResetPage } from "./email/RequestPasswordResetPage" with { type: "ref" };
import { SignupPage } from "./email/SignupPage" with { type: "ref" };

export const loginRoute = route("LoginRoute", "/login", page(LoginPage));

export const signupRoute = route("SignupRoute", "/signup", page(SignupPage));

export const emailVerificationRoute = route(
  "EmailVerificationRoute",
  "/email-verification",
  page(EmailVerificationPage),
);

export const requestPasswordResetRoute = route(
  "RequestPasswordResetRoute",
  "/request-password-reset",
  page(RequestPasswordResetPage),
);

export const passwordResetRoute = route(
  "PasswordResetRoute",
  "/password-reset",
  page(PasswordResetPage),
);

export const authSpec: Spec = [
  emailVerificationRoute,
  passwordResetRoute,
  loginRoute,
  signupRoute,
  requestPasswordResetRoute,
];
