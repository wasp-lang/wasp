import {
  action,
  app,
  customAuthHandler,
  page,
  query,
  route,
} from "@wasp.sh/spec";
import { MainPage } from "./src/MainPage" with { type: "ref" };
import { LoginPage } from "./src/auth/LoginPage" with { type: "ref" };
import { createClerkClientAuthHandler } from "./src/auth/clientAuthHandler" with { type: "ref" };
import { createClerkServerAuthHandler } from "./src/auth/handler" with { type: "ref" };
import { createTask, getMyTasks } from "./src/operations" with { type: "ref" };

export default app({
  name: "authProviderCustomClerk",
  wasp: { version: "^0.26.0" },
  title: "Auth providers — Custom handler (Clerk)",

  auth: {
    userEntity: "User",
    onAuthFailedRedirectTo: "/login",
    // Clerk's own token is the credential on every request, so the scheme
    // declares no `credentials`: Wasp issues nothing and adds no table.
    schemes: {
      clerk: customAuthHandler({
        // Both halves are factories from this app's own code: the same
        // things a handler package exports, with the same powers. Each side
        // holds what that half receives.
        server: {
          authHandlerFactory: createClerkServerAuthHandler,
          env: [
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
        },
        client: {
          authHandlerFactory: createClerkClientAuthHandler,
          // Declared here, so the client half receives it as `runtime.env`
          // and the app needs no client env schema of its own for it.
          env: [
            {
              name: "REACT_APP_CLERK_PUBLISHABLE_KEY",
              doc: "Clerk dashboard → API keys (publishable key)",
            },
          ],
        },
      }),
    },
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
