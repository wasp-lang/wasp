import { betterAuth } from "@wasp.sh/auth-better-auth/spec";
import { action, app, page, query, route } from "@wasp.sh/spec";
import { MainPage } from "./src/MainPage" with { type: "ref" };
import { LoginPage } from "./src/auth/LoginPage" with { type: "ref" };
import { setupBetterAuth } from "./src/auth/setup" with { type: "ref" };
import { createTask, getMyTasks } from "./src/operations" with { type: "ref" };

export default app({
  name: "authProviderBetterAuth",
  wasp: { version: "^0.26.0" },
  title: "Auth providers — Better Auth",

  auth: {
    userEntity: "User",
    onAuthFailedRedirectTo: "/login",
    // Wasp verifies every request through this adapter instead of its own
    // auth. Note what is absent: no methods, no hooks, no success redirect --
    // Better Auth owns signup and login entirely, and the provider union makes
    // wasp-auth-only configuration inexpressible here.
    //
    // `betterAuth()` is the adapter package's spec helper. Its manifest also
    // mounts Better Auth's own endpoints (sign-up, sign-in, OAuth callbacks)
    // at `/better-auth` on the Wasp server, with the JSON body parser
    // stripped -- Better Auth reads the raw request stream, and an
    // already-consumed stream hangs every request. In earlier versions of
    // this app that took an `api()` + `apiNamespace()` pair; the manifest's
    // `routes` declaration replaces both.
    // With a `setupFn`, the app owns the Better Auth configuration in full,
    // with Better Auth's own semantics (see the referenced module). Without
    // one, `betterAuth()` enables email-and-password auth by itself.
    provider: betterAuth({ setupFn: setupBetterAuth }),
  },

  spec: [
    // Identical to the other two apps.
    route("MainRoute", "/", page(MainPage, { authRequired: true })),
    route("LoginRoute", "/login", page(LoginPage)),
    query(getMyTasks, { entities: ["Task"], auth: true }),
    action(createTask, { entities: ["Task"], auth: true }),
  ],
});
