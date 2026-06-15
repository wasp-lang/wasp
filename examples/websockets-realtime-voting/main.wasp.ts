import { app, page, route } from "@wasp.sh/spec";

import { Layout } from "./src/Layout" with { type: "ref" };
import { LoginPage } from "./src/pages/LoginPage" with { type: "ref" };
import MainPage from "./src/pages/MainPage" with { type: "ref" };
import { SignupPage } from "./src/pages/SignupPage" with { type: "ref" };
import { votingWebSocket } from "./src/ws-server" with { type: "ref" };

export default app({
  name: "whereDoWeEat",
  wasp: { version: "^0.25.0" },
  title: "where-do-we-eat",
  head: ["<link rel='icon' href='/favicon.ico' />"],
  client: {
    rootComponent: Layout,
  },
  auth: {
    userEntity: "User",
    onAuthFailedRedirectTo: "/login",
    methods: {
      usernameAndPassword: {},
    },
  },
  webSocket: {
    fn: votingWebSocket,
  },
  spec: [
    route("RootRoute", "/", page(MainPage, { authRequired: true })),
    route("LoginRoute", "/login", page(LoginPage)),
    route("RegisterRoute", "/signup", page(SignupPage)),
  ],
});
