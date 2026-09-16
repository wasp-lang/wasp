import { waspAuth } from "@wasp.sh/auth/spec";
import { clerk } from "@wasp.sh/auth-clerk/spec";
import { action, app, page, query, route } from "@wasp.sh/spec";
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
    // human signing in through both gets two separate User rows. Each scheme
    // carries its own credential (Wasp's bearer token, Clerk's session
    // token), and `user.sessionScheme` says which scheme authenticated the
    // request.
    schemes: {
      wasp: waspAuth({
        methods: {
          usernameAndPassword: {},
        },
      }),
      clerk: clerk(),
    },
    // With more than one scheme, `authRequired: true` needs to know which
    // one it means.
    default: "wasp",
  },

  spec: [
    // A scheme list is tried in order; the first one that authenticates the
    // request wins. Both audiences get in here.
    route(
      "MainRoute",
      "/",
      page(MainPage, { authRequired: ["wasp", "clerk"] }),
    ),
    // A single scheme restricts the page to its credentials. A
    // Clerk-authenticated user sees an access-denied message here, not a
    // redirect loop.
    route("AdminRoute", "/admin", page(AdminPage, { authRequired: ["wasp"] })),
    route("LoginRoute", "/login", page(LoginPage)),
    query(getMyTasks, { entities: ["Task"], auth: ["wasp", "clerk"] }),
    action(createTask, { entities: ["Task"], auth: ["wasp", "clerk"] }),
    // The restricted form is self-enforcing server-side: no credential is a
    // 401, a credential of a non-listed scheme is a 403.
    query(getAdminReport, { entities: ["Task"], auth: ["wasp"] }),
  ],
});
