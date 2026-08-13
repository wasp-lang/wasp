import {
  action,
  api,
  apiNamespace,
  app,
  customAuthProvider,
  page,
  query,
  route,
} from "@wasp.sh/spec";
import { MainPage } from "./src/MainPage" with { type: "ref" };
import { LoginPage } from "./src/auth/LoginPage" with { type: "ref" };
import { rawBodyMiddleware } from "./src/auth/middleware" with { type: "ref" };
import { betterAuthProvider } from "./src/auth/provider" with { type: "ref" };
import { betterAuthRoutes } from "./src/auth/routes" with { type: "ref" };
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
    provider: customAuthProvider({
      id: "better-auth",
      server: betterAuthProvider,
      capabilities: ["session-revocation"],
    }),
  },

  spec: [
    // Identical to the other two apps.
    route("MainRoute", "/", page(MainPage, { authRequired: true })),
    route("LoginRoute", "/login", page(LoginPage)),
    query(getMyTasks, { entities: ["Task"], auth: true }),
    action(createTask, { entities: ["Task"], auth: true }),

    // Better Auth's own endpoints, mounted inside the Wasp server.
    //
    // `toNodeHandler` reads the raw request stream, so this namespace strips the
    // JSON body parser Wasp mounts by default. Getting that wrong is a genuinely
    // confusing failure: Better Auth sees an already-consumed stream and every
    // request hangs.
    apiNamespace("/better-auth", { middlewareConfigFn: rawBodyMiddleware }),
    api("ALL", "/better-auth/*splat", betterAuthRoutes, {
      auth: false,
      entities: [],
    }),
  ],
});
