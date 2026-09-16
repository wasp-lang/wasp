import {
  action,
  app,
  customAuthHandler,
  page,
  query,
  route,
} from "@wasp.sh/spec";
import { App } from "./src/App" with { type: "ref" };
import { MainPage } from "./src/MainPage" with { type: "ref" };
import { LoginPage } from "./src/auth/LoginPage" with { type: "ref" };
import { clerkAuthHandler } from "./src/auth/handler" with { type: "ref" };
import { clientEnvSchema } from "./src/env" with { type: "ref" };
import { createTask, getMyTasks } from "./src/operations" with { type: "ref" };

export default app({
  name: "authProviderCustomClerk",
  wasp: { version: "^0.26.0" },
  title: "Auth providers — Custom adapter (Clerk)",

  auth: {
    userEntity: "User",
    onAuthFailedRedirectTo: "/login",
    // Clerk's own token is the credential on every request, so the scheme
    // declares no `credentials`: Wasp issues nothing and adds no table.
    schemes: {
      clerk: customAuthHandler({
        server: clerkAuthHandler,
        env: {
          server: [
            { name: "CLERK_SECRET_KEY", doc: "Clerk dashboard → API keys" },
            {
              name: "CLERK_PUBLISHABLE_KEY",
              doc: "Clerk dashboard → API keys",
            },
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
  },

  client: {
    // Wraps the app in Clerk's provider and registers its token as the
    // credential Wasp's client puts on every request.
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
