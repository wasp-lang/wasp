import { clerk } from "@wasp.sh/auth-clerk/spec";
import { action, app, page, query, route } from "@wasp.sh/spec";
import { App } from "./src/App" with { type: "ref" };
import { MainPage } from "./src/MainPage" with { type: "ref" };
import { LoginPage } from "./src/auth/LoginPage" with { type: "ref" };
import { clientEnvSchema } from "./src/env" with { type: "ref" };
import { createTask, getMyTasks } from "./src/operations" with { type: "ref" };

export default app({
  name: "authProviderClerk",
  wasp: { version: "^0.26.0" },
  title: "Auth providers — Clerk",

  auth: {
    userEntity: "User",
    onAuthFailedRedirectTo: "/login",
    // Clerk owns signup and login entirely; Wasp has no auth methods of its
    // own here, and the provider union makes them inexpressible.
    //
    // `clerk()` is the adapter package's spec helper. It produces the whole
    // manifest -- server entry, capabilities, required env vars -- so the app
    // declares nothing but the choice. Compare `customAuthProvider`, the
    // escape hatch this package is built on.
    provider: clerk(),
  },

  client: {
    // Wraps the app in Clerk's provider and bridges its token to Wasp's client.
    rootComponent: App,
    envValidationSchema: clientEnvSchema,
  },

  spec: [
    // Identical to the other two apps.
    route("MainRoute", "/", page(MainPage, { authRequired: true })),
    route("LoginRoute", "/login", page(LoginPage)),
    query(getMyTasks, { entities: ["Task"], auth: true }),
    action(createTask, { entities: ["Task"], auth: true }),
    // Note: no api() declarations. Clerk contributes no routes and no tables.
  ],
});
