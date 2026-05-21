import { action, app, page, query, route } from "@wasp.sh/spec";
import { App } from "./src/App" with { type: "ref " };
import { EmailVerificationPage } from "./src/auth/email/EmailVerificationPage" with { type: "ref " };
import { LoginPage } from "./src/auth/email/LoginPage" with { type: "ref " };
import { PasswordResetPage } from "./src/auth/email/PasswordResetPage" with { type: "ref " };
import { RequestPasswordResetPage } from "./src/auth/email/RequestPasswordResetPage" with { type: "ref " };
import { SignupPage } from "./src/auth/email/SignupPage" with { type: "ref " };
import { userSignupFields } from "./src/auth/email/userSignupFields" with { type: "ref " };
import { createTag } from "./src/tags/actions" with { type: "ref " };
import { getTags } from "./src/tags/queries" with { type: "ref " };
import { TasksPage } from "./src/tasks/TasksPage" with { type: "ref " };
import {
  createTask,
  deleteCompletedTasks,
  updateTaskStatus,
} from "./src/tasks/actions" with { type: "ref " };
import { getTasks } from "./src/tasks/queries" with { type: "ref " };

export default app({
  name: "__waspAppName__",
  title: "__waspProjectName__",
  wasp: { version: "__waspVersion__" },
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
          clientRoute: "EmailVerificationRoute",
        },
        passwordReset: {
          clientRoute: "PasswordResetRoute",
        },
      },
    },
    onAuthSucceededRedirectTo: "/",
    onAuthFailedRedirectTo: "/login",
  },
  emailSender: {
    provider: "Dummy",
  },
  client: {
    rootComponent: App,
  },
  parts: [
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

    route("TasksRoute", "/", page(TasksPage, { authRequired: true })),
    query(getTasks, { entities: ["Task", "Tag"] }),
    action(createTask, { entities: ["Task"] }),
    action(updateTaskStatus, { entities: ["Task"] }),
    action(deleteCompletedTasks, { entities: ["Task"] }),

    query(getTags, { entities: ["Tag"] }),
    action(createTag, { entities: ["Tag"] }),
  ],
});
