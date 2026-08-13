import {
  action,
  app,
  customAuthProvider,
  page,
  query,
  route,
} from "@wasp.sh/spec";
import { App } from "./src/App" with { type: "ref" };
import { MainPage } from "./src/MainPage" with { type: "ref" };
import { LoginPage } from "./src/auth/LoginPage" with { type: "ref" };
import { clerkAuthProvider } from "./src/auth/provider" with { type: "ref" };
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
    provider: customAuthProvider({
      id: "clerk",
      server: clerkAuthProvider,
      capabilities: ["session-revocation"],
      // Rendered into the generated env validation: a missing var fails at
      // boot with this explanation, not at the first authenticated request.
      env: {
        server: [
          { name: "CLERK_SECRET_KEY", doc: "Clerk dashboard → API keys" },
          { name: "CLERK_PUBLISHABLE_KEY", doc: "Clerk dashboard → API keys" },
          {
            name: "CLERK_JWT_KEY",
            optional: true,
            doc: "enables networkless JWT verification",
          },
        ],
        client: [],
      },
    }),
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
