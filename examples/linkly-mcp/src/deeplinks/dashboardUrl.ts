// The dashboard-URL tool's operation. Only MCP calls it: with `expose.mcp`
// it'd set `expose.client: false`.
import type { GetDashboardUrl } from "wasp/server/operations";
import { DomainError } from "../shared/errors";
import { runOperation } from "../shared/waspErrors";
import type { DashboardUrlInput, DashboardUrlOut } from "./schemas";
import { dashboardUrls } from "./urls";

export const getDashboardUrl: GetDashboardUrl<
  DashboardUrlInput,
  DashboardUrlOut
> = ({ view, id, params = {} }) =>
  runOperation(async () => {
    const needsId = view === "link" || view === "doc";
    if (needsId && !id)
      throw new DomainError(
        "invalid",
        `The ${view} view needs an id${view === "link" ? " (link ID or slug)" : " (doc slug)"}.`,
      );
    const url = (() => {
      switch (view) {
        case "links":
          return dashboardUrls.links({
            search: params.search,
            tag: params.tag,
          });
        case "link":
          return dashboardUrls.link(id!, {
            tab:
              params.tab ??
              (params.range || params.groupBy ? "stats" : undefined),
            range: params.range,
            groupBy: params.groupBy,
          });
        case "newLink":
          return dashboardUrls.newLink({
            destinationUrl: params.destinationUrl,
            slug: params.slug,
            title: params.title,
            tags: params.tags,
          });
        case "topLinks":
          return dashboardUrls.topLinks({ range: params.range });
        case "connections":
          return dashboardUrls.connections();
        case "doc":
          return dashboardUrls.doc(id!);
      }
    })();
    return { url, view };
  });
