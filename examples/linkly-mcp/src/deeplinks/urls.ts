// Pure URL builders for every dashboard view (no I/O). Every entity returned
// by any surface carries a `dashboardUrl` built here, and `get_dashboard_url`
// exposes them to agents. The pages must honor every parameter produced.
import type { GroupBy, Range } from "../analytics/ranges";
import { frontendUrl } from "../shared/urls";

export type LinkPrefill = {
  destinationUrl?: string;
  slug?: string;
  title?: string;
  tags?: string[];
};

type Query = Record<string, string | string[] | undefined>;

function withQuery(path: string, query: Query = {}): string {
  const params = new URLSearchParams();
  for (const [key, value] of Object.entries(query)) {
    if (value === undefined || value === "") continue;
    if (Array.isArray(value)) {
      if (value.length) params.set(key, value.join(","));
    } else {
      params.set(key, value);
    }
  }
  const qs = params.toString();
  return qs ? `${path}?${qs}` : path;
}

/** Relative paths: used by the dashboard's own navigation. */
export const dashboardPaths = {
  links: (q?: { search?: string; tag?: string }) => withQuery("/links", q),
  link: (
    id: string,
    q?: { tab?: "edit" | "stats"; range?: Range; groupBy?: GroupBy },
  ) => withQuery(`/links/${encodeURIComponent(id)}`, q),
  newLink: (prefill?: LinkPrefill) => withQuery("/links/new", prefill),
  topLinks: (q?: { range?: Range }) => withQuery("/analytics", q),
  connections: () => "/settings/connections",
  doc: (slug: string) => `/docs/${encodeURIComponent(slug)}`,
};

/** Absolute URLs: what APIs, MCP tools, and the CLI return. */
export const dashboardUrls: {
  [K in keyof typeof dashboardPaths]: (
    ...args: Parameters<(typeof dashboardPaths)[K]>
  ) => string;
} = {
  links: (q) => frontendUrl() + dashboardPaths.links(q),
  link: (id, q) => frontendUrl() + dashboardPaths.link(id, q),
  newLink: (prefill) => frontendUrl() + dashboardPaths.newLink(prefill),
  topLinks: (q) => frontendUrl() + dashboardPaths.topLinks(q),
  connections: () => frontendUrl() + dashboardPaths.connections(),
  doc: (slug) => frontendUrl() + dashboardPaths.doc(slug),
};

/** Route patterns each builder targets; checked against the `*.wasp.ts` files by a test. */
export const DASHBOARD_ROUTE_PATTERNS: Record<
  keyof typeof dashboardPaths,
  string
> = {
  links: "/links",
  link: "/links/:id",
  newLink: "/links/new",
  topLinks: "/analytics",
  connections: "/settings/connections",
  doc: "/docs/:slug",
};
