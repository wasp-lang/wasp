import { clerk } from "@wasp.sh/auth-clerk/spec";
import { action, app, page, query, route, waspAuth } from "@wasp.sh/spec";
import { AdminPage } from "./src/AdminPage" with { type: "ref" };
import { MainPage } from "./src/MainPage" with { type: "ref" };
import { LoginPage } from "./src/auth/LoginPage" with { type: "ref" };
import {
  createTask,
  getAdminReport,
  getMyTasks,
} from "./src/operations" with { type: "ref" };

export default app({
  name: "authProviderMulti",
  wasp: { version: "^0.26.0" },
  title: "Auth providers — waspAuth + Clerk",

  auth: {
    userEntity: "User",
    onAuthFailedRedirectTo: "/login",
    // Two independent identity systems at once. No account linking: the same
    // human signing in through both gets two separate User rows. Every login,
    // whichever provider, ends in the same Wasp session, and
    // `user.sessionProviderId` says which provider minted it.
    providers: [
      waspAuth({
        methods: {
          usernameAndPassword: {},
        },
      }),
      clerk(),
    ],
  },

  spec: [
    // `authRequired: true` means ANY provider: a valid Wasp session, however
    // it was minted.
    route("MainRoute", "/", page(MainPage, { authRequired: true })),
    // A provider list restricts the page to sessions minted by those
    // providers. A Clerk-authenticated user sees an access-denied message
    // here, not a redirect loop.
    route("AdminRoute", "/admin", page(AdminPage, { authRequired: ["wasp"] })),
    route("LoginRoute", "/login", page(LoginPage)),
    query(getMyTasks, { entities: ["Task"], auth: true }),
    action(createTask, { entities: ["Task"], auth: true }),
    // The restricted form is self-enforcing server-side: no session is a 401,
    // a session from a non-listed provider is a 403.
    query(getAdminReport, { entities: ["Task"], auth: ["wasp"] }),
  ],
});
