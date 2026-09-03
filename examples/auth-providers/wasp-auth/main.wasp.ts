import { waspAuth } from "@wasp.sh/auth/spec";
import { action, app, page, query, route } from "@wasp.sh/spec";
import { MainPage } from "./src/MainPage" with { type: "ref" };
import { LoginPage } from "./src/auth/LoginPage" with { type: "ref" };
import { createTask, getMyTasks } from "./src/operations" with { type: "ref" };

export default app({
  name: "authProviderWaspAuth",
  wasp: { version: "^0.26.0" },
  title: "Auth providers — Wasp auth",

  auth: {
    userEntity: "User",
    onAuthFailedRedirectTo: "/login",
    // Wasp's own auth is an adapter package like Better Auth or Clerk: the
    // compiler knows nothing about it beyond its manifest.
    providers: [
      waspAuth({
        methods: {
          usernameAndPassword: {},
        },
        onAuthSucceededRedirectTo: "/",
      }),
    ],
  },

  spec: [
    route("MainRoute", "/", page(MainPage, { authRequired: true })),
    route("LoginRoute", "/login", page(LoginPage)),
    query(getMyTasks, { entities: ["Task"], auth: true }),
    action(createTask, { entities: ["Task"], auth: true }),
  ],
});
