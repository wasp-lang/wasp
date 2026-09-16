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
import { passwordAuthHandler } from "./src/auth/handler" with { type: "ref" };
import { login } from "./src/auth/loginApi" with { type: "ref" };
import { signup } from "./src/auth/signupApi" with { type: "ref" };
import { createTask, getMyTasks } from "./src/operations" with { type: "ref" };

export default app({
  name: "authProviderCustomPassword",
  wasp: { version: "^0.26.0" },
  title: "Auth providers — Custom password auth",

  auth: {
    userEntity: "User",
    onAuthFailedRedirectTo: "/login",
    // A hand-rolled email+password scheme, built from the same primitives
    // every scheme gets: the identities facet for storage, the credentials
    // facet for signing in, and `api()` routes for signup and login.
    // `credentials: {}` asks Wasp for the default private issuer (a bearer
    // token backed by the Session table); `{ transport: "cookie" }` or
    // `{ store: "signed-token" }` would change that without touching the
    // handler.
    schemes: {
      password: customAuthHandler({
        server: passwordAuthHandler,
        credentials: {},
      }),
    },
  },

  spec: [
    route("MainRoute", "/", page(MainPage, { authRequired: true })),
    route("LoginRoute", "/login", page(LoginPage)),
    // The scheme's own signup and login endpoints -- ordinary Wasp api routes.
    api("POST", "/password-auth/signup", signup, { auth: false, entities: [] }),
    api("POST", "/password-auth/login", login, { auth: false, entities: [] }),
    query(getMyTasks, { entities: ["Task"], auth: true }),
    action(createTask, { entities: ["Task"], auth: true }),
  ],
});
