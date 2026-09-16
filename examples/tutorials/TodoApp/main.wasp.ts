import { action, app, page, query, route, waspAuth } from "@wasp.sh/spec";

import { createTask, updateTask } from "./src/actions" with { type: "ref" };
import { LoginPage } from "./src/LoginPage" with { type: "ref" };
import { MainPage } from "./src/MainPage" with { type: "ref" };
import { getTasks } from "./src/queries" with { type: "ref" };
import { SignupPage } from "./src/SignupPage" with { type: "ref" };

export default app({
  name: "TodoApp",
  wasp: { version: "0.26.0" },
  title: "TodoApp",
  head: ["<link rel='icon' href='/favicon.ico' />"],
  auth: {
    userEntity: "User",
    onAuthFailedRedirectTo: "/login",
    providers: [
      waspAuth({
        methods: {
          usernameAndPassword: {},
        },
      }),
    ],
  },
  spec: [
    route("RootRoute", "/", page(MainPage, { authRequired: true })),
    route("SignupRoute", "/signup", page(SignupPage)),
    route("LoginRoute", "/login", page(LoginPage)),

    query(getTasks, { entities: ["Task"] }),
    action(createTask, { entities: ["Task"] }),
    action(updateTask, { entities: ["Task"] }),
  ],
});
