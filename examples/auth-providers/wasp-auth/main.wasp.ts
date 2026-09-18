import { waspAuth } from "@wasp.sh/auth/spec";
import { action, app, page, query, route } from "@wasp.sh/spec";
import { MainPage } from "./src/MainPage" with { type: "ref" };
import { ConnectedAccountsPage } from "./src/auth/ConnectedAccountsPage" with { type: "ref" };
import { onBeforeLink } from "./src/auth/hooks" with { type: "ref" };
import { LoginPage } from "./src/auth/LoginPage" with { type: "ref" };
import { mergeUsers } from "./src/auth/mergeUsers" with { type: "ref" };
import { createTask, getMyTasks } from "./src/operations" with { type: "ref" };

export default app({
  name: "authProviderWaspAuth",
  wasp: { version: "^0.26.0" },
  title: "Auth providers — Wasp auth",

  auth: {
    userEntity: "User",
    onAuthFailedRedirectTo: "/login",
    // Wasp's own auth is a handler package like Better Auth or Clerk: the
    // compiler knows nothing about it beyond its manifest. The scheme name
    // ('wasp') prefixes its routes (/auth/wasp/...) and identity namespaces
    // (wasp:username). Being the only scheme, it is the default one.
    schemes: {
      wasp: waspAuth({
        methods: {
          usernameAndPassword: {},
        },
        onAuthSucceededRedirectTo: "/",
      }),
    },
    // App-level, like the login and signup hooks: linking is about the user,
    // not about one handler.
    hooks: { onBeforeLink },
    // Turns account merging on. Without it, a login that belongs to another
    // account simply cannot be linked.
    mergeUsers,
  },

  spec: [
    route("MainRoute", "/", page(MainPage, { authRequired: true })),
    route("LoginRoute", "/login", page(LoginPage)),
    route(
      "ConnectedAccountsRoute",
      "/accounts",
      page(ConnectedAccountsPage, { authRequired: true }),
    ),
    query(getMyTasks, { entities: ["Task"], auth: true }),
    action(createTask, { entities: ["Task"], auth: true }),
  ],
});
