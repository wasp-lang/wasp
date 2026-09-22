import {
  action,
  api,
  app,
  customAuthHandler,
  page,
  query,
  route,
} from "@wasp.sh/spec";
import { MainPage } from "./src/MainPage" with { type: "ref" };
import { LoginPage } from "./src/auth/LoginPage" with { type: "ref" };
import { createPasswordAuthHandler } from "./src/auth/handler" with { type: "ref" };
import { createTask, getMyTasks } from "./src/operations" with { type: "ref" };
import {
  signInAs,
  signOutEverywhereRoute,
  signOutHere,
  whoAmI,
} from "./src/imperative" with { type: "ref" };

export default app({
  name: "authProviderCustomPassword",
  wasp: { version: "^0.26.0" },
  title: "Auth providers — Custom password auth",

  auth: {
    userEntity: "User",
    onAuthFailedRedirectTo: "/login",
    // A hand-rolled email+password scheme. `server.authAdapter` is the
    // same thing a handler package exports, so it gets the scheme's runtime as
    // an argument and brings its own routes (mounted at /auth/password).
    // `credentials: {}` asks Wasp for the default private issuer (a bearer
    // token backed by the Session table); `{ transport: "cookie" }` or
    // `{ store: "signed-token" }` would change that without touching the
    // handler.
    schemes: {
      password: customAuthHandler({
        server: {
          authAdapter: createPasswordAuthHandler,
          routes: {},
        },
        credentials: {},
      }),
    },
  },

  spec: [
    route("MainRoute", "/", page(MainPage, { authRequired: true })),
    route("LoginRoute", "/login", page(LoginPage)),
    query(getMyTasks, { entities: ["Task"], auth: true }),
    action(createTask, { entities: ["Task"], auth: true }),
    // The imperative auth API, from plain routes.
    api("POST", "/api/sign-in-as", signInAs, { auth: false }),
    api("GET", "/api/whoami", whoAmI, { auth: false }),
    api("POST", "/api/sign-out", signOutHere, { auth: false }),
    api("POST", "/api/sign-out-everywhere", signOutEverywhereRoute, {
      auth: true,
    }),
  ],
});
