import { app, page, route } from "@wasp.sh/spec";

import { tagsSpec } from "./src/tags/tags.wasp";
import { tasksPage, tasksSpec } from "./src/tasks/tasks.wasp";

import { App } from "./src/App" with { type: "ref" };
import { EmailVerificationPage } from "./src/auth/email/EmailVerificationPage" with { type: "ref" };
import { LoginPage } from "./src/auth/email/LoginPage" with { type: "ref" };
import { PasswordResetPage } from "./src/auth/email/PasswordResetPage" with { type: "ref" };
import { RequestPasswordResetPage } from "./src/auth/email/RequestPasswordResetPage" with { type: "ref" };
import { SignupPage } from "./src/auth/email/SignupPage" with { type: "ref" };
import { userSignupFields } from "./src/auth/email/userSignupFields" with { type: "ref" };

const mainRoute = route("TasksRoute", "/", tasksPage);

export default app({
  name: "__waspAppName__",
  wasp: { version: "__waspVersion__" },
  title: "__waspProjectName__",
  head: ["<link rel='icon' href='/favicon.ico' />"],
  auth: {
    userEntity: "User",
    methods: {
      email: {
        fromField: {
          name: "Basic App",
          email: "hello@example.com",
        },
        userSignupFields,
        emailVerification: {
          clientRoute: route(
            "EmailVerificationRoute",
            "/email-verification",
            page(EmailVerificationPage),
          ),
        },
        passwordReset: {
          clientRoute: route(
            "PasswordResetRoute",
            "/password-reset",
            page(PasswordResetPage),
          ),
        },
      },
    },
    onAuthSucceededRedirectTo: mainRoute,
    onAuthFailedRedirectTo: route("LoginRoute", "/login", page(LoginPage)),
  },
  emailSender: {
    provider: "Dummy",
  },
  client: {
    rootComponent: App,
  },
  spec: [
    mainRoute,
    tasksSpec,
    tagsSpec,
    route("SignupRoute", "/signup", page(SignupPage)),
    route(
      "RequestPasswordResetRoute",
      "/request-password-reset",
      page(RequestPasswordResetPage),
    ),
  ],
});
