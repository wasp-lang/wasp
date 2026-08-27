import {
  action,
  api,
  app,
  customAuthProvider,
  page,
  query,
  route,
} from "@wasp.sh/spec";
import { MainPage } from "./src/MainPage" with { type: "ref" };
import { LoginPage } from "./src/auth/LoginPage" with { type: "ref" };
import { passwordAuthProvider } from "./src/auth/provider" with { type: "ref" };
import { signup } from "./src/auth/signupApi" with { type: "ref" };
import { createTask, getMyTasks } from "./src/operations" with { type: "ref" };

export default app({
  name: "authProviderCustomPassword",
  wasp: { version: "^0.26.0" },
  title: "Auth providers — Custom password auth",

  auth: {
    userEntity: "User",
    onAuthFailedRedirectTo: "/login",
    // A hand-rolled email+password provider, built from the same three
    // primitives every provider gets: the identity store for storage, the
    // `POST /auth/login` exchange for sessions, and an `api()` route for
    // signup. No capabilities: a stateless verifier has no provider session
    // to issue or revoke -- Wasp's own session is the only one.
    provider: customAuthProvider({
      id: "external:password",
      server: passwordAuthProvider,
      capabilities: [],
      env: { server: [], client: [] },
    }),
  },

  spec: [
    route("MainRoute", "/", page(MainPage, { authRequired: true })),
    route("LoginRoute", "/login", page(LoginPage)),
    // The provider's own signup endpoint -- an ordinary Wasp api route.
    api("POST", "/password-auth/signup", signup, { auth: false, entities: [] }),
    query(getMyTasks, { entities: ["Task"], auth: true }),
    action(createTask, { entities: ["Task"], auth: true }),
  ],
});
