// The dashboard-URL tool's schemas.
import { GROUP_BYS, RANGES } from "../analytics/ranges";
import { z } from "../shared/zod";

export const VIEWS = [
  "links",
  "link",
  "newLink",
  "topLinks",
  "connections",
  "doc",
] as const;

export const DashboardUrlInput = z.object({
  view: z
    .enum(VIEWS)
    .describe(
      "Which page: links, link, newLink, topLinks, connections, or doc (see the tool description).",
    ),
  id: z
    .string()
    .max(200)
    .optional()
    .describe(
      "Required for link (the link's ID or slug) and doc (the doc slug).",
    ),
  params: z
    .object({
      search: z.string().optional().describe("links: prefill the search box."),
      tag: z.string().optional().describe("links: filter by this tag."),
      tab: z
        .enum(["edit", "stats"])
        .optional()
        .describe("link: open the edit tab or the stats tab."),
      range: z
        .enum(RANGES)
        .optional()
        .describe(
          "link (stats) and topLinks: time range 24h, 7d, 30d, or 90d.",
        ),
      groupBy: z
        .enum(GROUP_BYS)
        .optional()
        .describe("link (stats): chart by day or by referrer."),
      destinationUrl: z
        .string()
        .optional()
        .describe("newLink: prefill the destination URL."),
      slug: z.string().optional().describe("newLink: prefill the slug."),
      title: z.string().optional().describe("newLink: prefill the title."),
      tags: z.array(z.string()).optional().describe("newLink: prefill tags."),
    })
    .optional()
    .describe("Optional view-specific options; unrelated ones are ignored."),
});
export type DashboardUrlInput = z.infer<typeof DashboardUrlInput>;

export const DashboardUrlOut = z.object({
  url: z.string().describe("The dashboard URL to open or share with the user."),
  view: z.enum(VIEWS).describe("The view the URL opens."),
});
export type DashboardUrlOut = z.infer<typeof DashboardUrlOut>;
