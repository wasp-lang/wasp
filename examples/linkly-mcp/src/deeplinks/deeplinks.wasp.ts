import { page, query, route, type Spec } from "@wasp/spec";
import { getDashboardUrl } from "./dashboardUrl" with { type: "ref" };
import {
  DashboardUrlInput,
  DashboardUrlOut,
} from "./schemas" with { type: "ref" };
import { ConfirmActionPage } from "./ui/ConfirmActionPage" with { type: "ref" };

export const deeplinksSpec: Spec = [
  // app.mcp.confirmRoute: where users approve destructive tool calls. Wasp
  // sends logged-out users to /login and brings them back here.
  route(
    "ConfirmRoute",
    "/confirm",
    page(ConfirmActionPage, { authRequired: true }),
  ),
  // MCP only: no client route or export.
  query(getDashboardUrl, {
    expose: {
      client: false,
      mcp: {
        title: "Get a Linkly dashboard URL",
        description:
          "Use this when the user wants to see, visualize, or manually edit something in Linkly: it returns a URL to open. Views: links = the link list (optional search/tag filter); link = one link (id required; tab edit or stats, with range/groupBy for the chart); newLink = the create form, prefilled with destinationUrl/slug/title/tags; topLinks = most-clicked links for a range; connections = connected apps; doc = a docs page (id = doc slug from search_docs). Results from list_links, get_link, and get_link_stats already include a dashboardUrl.",
        input: DashboardUrlInput,
        output: DashboardUrlOut,
        idempotent: true,
      },
    },
  }),
];
