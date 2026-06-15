import { action, app, page, query, route } from "@wasp.sh/spec";

import { createTask, updateTask } from "./src/actions" with { type: "ref" };
import { LoginPage } from "./src/LoginPage" with { type: "ref" };
import { MainPage } from "./src/MainPage" with { type: "ref" };
import { getTasks } from "./src/queries" with { type: "ref" };
import { SignupPage } from "./src/SignupPage" with { type: "ref" };

export default app({
  name: "TodoApp",
  wasp: { version: "^0.25.0" },
  title: "TodoApp",
  head: ["<link rel='icon' href='/favicon.ico' />"],
  auth: {
    userEntity: "User",
    methods: {
      usernameAndPassword: {},
    },
    onAuthFailedRedirectTo: "/login",
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
