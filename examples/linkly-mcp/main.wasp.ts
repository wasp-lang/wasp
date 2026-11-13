import { app } from "@wasp/spec";
import { analyticsSpec } from "./src/analytics/analytics.wasp";
import { appSpec } from "./src/app/app.wasp";
import { Layout } from "./src/app/Layout" with { type: "ref" };
import { deeplinksSpec } from "./src/deeplinks/deeplinks.wasp";
import { docsSpec } from "./src/docs/docs.wasp";
import { linksSpec } from "./src/links/links.wasp";
import { oauthSpec } from "./src/oauth/oauth.wasp";
import { SCOPES } from "./src/shared/scopes";
import { serverSetup } from "./src/shared/serverSetup" with { type: "ref" };
import { testingSpec } from "./src/testing/testing.wasp";

// The Wasp Spec only joins the slices. Each slice declares its own pages,
// routes, operations, and APIs in its `<slice>.wasp.ts` file.
export default app({
  name: "linkly",
  wasp: { version: "^0.27.0" },
  title: "Linkly",
  head: ["<link rel='icon' href='/favicon.ico' />"],
  auth: {
    userEntity: "User",
    methods: {
      usernameAndPassword: {},
    },
    onAuthFailedRedirectTo: "/login",
    onAuthSucceededRedirectTo: "/links",
  },
  // The MCP server: tools come from `expose.mcp` on the operations.
  mcp: {
    instructions: [
      "Linkly is a link shortener with click analytics: you can list, create, update, and delete short links, and read how often they're clicked.",
      "Use search_docs when you're unsure how a Linkly concept works (slugs, tags, how clicks are counted).",
      "Results include a dashboardUrl; use get_dashboard_url or share those URLs when the user wants to see charts or edit something themselves.",
      "Prefer update_link with archived: true over delete_link when the user might want the link back.",
      "Deleting a link waits for the user's approval in Linkly. Clients that can open web pages show the approval page themselves; otherwise delete_link returns a confirmUrl to share with the user, and the link is deleted once they approve. Call delete_link again with the same id to learn the outcome.",
    ].join(" "),
    consentRoute: "OAuthAuthorizeRoute",
    // delete_link waits for the user's approval here.
    confirmRoute: "ConfirmRoute",
    // One checkbox each on the consent screen; tools name the one they need.
    scopes: SCOPES,
  },
  client: { rootComponent: Layout },
  server: { setupFn: serverSetup },
  spec: [
    appSpec,
    linksSpec,
    analyticsSpec,
    oauthSpec,
    docsSpec,
    deeplinksSpec,
    testingSpec,
  ],
});
