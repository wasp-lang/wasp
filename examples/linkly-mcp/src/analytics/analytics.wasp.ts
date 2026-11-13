import { api, page, query, route, type Spec } from "@wasp/spec";
import { getLinkStats, getTopLinks } from "./operations" with { type: "ref" };
import { redirect } from "./redirect" with { type: "ref" };
import {
  GetLinkStatsInput,
  GetTopLinksInput,
  LinkStats,
  TopLinks,
} from "./schemas" with { type: "ref" };
import { TopLinksPage } from "./ui/TopLinksPage" with { type: "ref" };

export const analyticsSpec: Spec = [
  // Public short URLs
  api("GET", "/r/:slug", redirect, { auth: false }),
  // Dashboard
  route(
    "TopLinksRoute",
    "/analytics",
    page(TopLinksPage, { authRequired: true }),
  ),
  query(getLinkStats, {
    entities: ["Link", "Click"],
    expose: {
      mcp: {
        title: "Get click stats for a link",
        description:
          "Returns click counts for one link (by ID or slug), either per day (zero-filled, oldest first) or per referring website. The returned dashboardUrl opens an interactive chart of the same data in Linkly: share it with the user when they want to see or explore the numbers. Use get_top_links to compare links.",
        input: GetLinkStatsInput,
        output: LinkStats,
        idempotent: true,
        scope: "analytics:read",
      },
    },
  }),
  query(getTopLinks, {
    entities: ["Link", "Click"],
    expose: {
      mcp: {
        title: "Get most-clicked links",
        description:
          "Ranks the user's links by clicks in a time window, most first (links with no clicks are left out). The returned dashboardUrl opens the same ranking in Linkly; suggest it when the user wants to browse. Use get_link_stats for one link's daily or referrer breakdown.",
        input: GetTopLinksInput,
        output: TopLinks,
        idempotent: true,
        scope: "analytics:read",
      },
    },
  }),
];
