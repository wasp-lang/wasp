import { app, route } from "@wasp.sh/spec";

import {
  authSpec,
  emailVerificationRoute,
  loginRoute,
  passwordResetRoute,
} from "./src/auth/auth.wasp";
import { tagsSpec } from "./src/tags/tags.wasp";
import { tasksPage, tasksSpec } from "./src/tasks/tasks.wasp";

import { App } from "./src/App" with { type: "ref" };
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
          clientRoute: emailVerificationRoute,
        },
        passwordReset: {
          clientRoute: passwordResetRoute,
        },
      },
    },
    onAuthSucceededRedirectTo: mainRoute,
    onAuthFailedRedirectTo: loginRoute,
  },
  emailSender: {
    provider: "Dummy",
  },
  client: {
    rootComponent: App,
  },
  spec: [mainRoute, tasksSpec, tagsSpec, authSpec],
});
