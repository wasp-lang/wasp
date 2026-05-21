import { type Auth, type Part, page, route } from "@wasp.sh/spec";
import { EmailVerificationPage } from "./email/EmailVerificationPage" with { type: "ref " };
import { LoginPage } from "./email/LoginPage" with { type: "ref " };
import { PasswordResetPage } from "./email/PasswordResetPage" with { type: "ref " };
import { RequestPasswordResetPage } from "./email/RequestPasswordResetPage" with { type: "ref " };
import { SignupPage } from "./email/SignupPage" with { type: "ref " };
import { userSignupFields } from "./email/userSignupFields" with { type: "ref " };

export const auth: Auth = {
  userEntity: "User",
  methods: {
    email: {
      fromField: {
        name: "Basic App",
        email: "hello@example.com",
      },
      userSignupFields,
      emailVerification: {
        clientRoute: "EmailVerificationRoute",
      },
      passwordReset: {
        clientRoute: "PasswordResetRoute",
      },
    },
  },
  onAuthSucceededRedirectTo: "/",
  onAuthFailedRedirectTo: "/login",
};

export const authParts: Part[] = [
  route("LoginRoute", "/login", page(LoginPage)),
  route("SignupRoute", "/signup", page(SignupPage)),
  route(
    "RequestPasswordResetRoute",
    "/request-password-reset",
    page(RequestPasswordResetPage),
  ),
  route("PasswordResetRoute", "/password-reset", page(PasswordResetPage)),
  route(
    "EmailVerificationRoute",
    "/email-verification",
    page(EmailVerificationPage),
  ),
];
