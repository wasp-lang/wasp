import { page, route, type Spec } from "@wasp/spec";
import { ConnectionsPage } from "./ui/ConnectionsPage" with { type: "ref" };
import { ConsentPage } from "./ui/ConsentPage" with { type: "ref" };

// The MCP endpoint, the OAuth token and revocation endpoints, and the
// metadata are generated from app.mcp. The app only provides these pages.
export const oauthSpec: Spec = [
  // app.mcp.consentRoute: the OAuth authorization endpoint. Wasp sends
  // logged-out users to /login and brings them back here.
  route(
    "OAuthAuthorizeRoute",
    "/oauth/authorize",
    page(ConsentPage, { authRequired: true }),
  ),
  // Connected apps
  route(
    "ConnectionsRoute",
    "/settings/connections",
    page(ConnectionsPage, { authRequired: true }),
  ),
];
