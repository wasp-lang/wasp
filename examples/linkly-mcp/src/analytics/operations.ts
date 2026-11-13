import { prisma } from "wasp/server";
import type { GetLinkStats, GetTopLinks } from "wasp/server/operations";
import { runOperation } from "../shared/waspErrors";
import { callerPrincipal } from "../shared/waspSession";
import type { LinkStats, StatsQuery, TopLinks, TopLinksQuery } from "./schemas";
import * as analytics from "./service";

const deps = { prisma };

export const getLinkStats: GetLinkStats<StatsQuery, LinkStats> = (
  args,
  context,
) =>
  runOperation(() =>
    analytics.getLinkStats(callerPrincipal(context), args, deps),
  );

export const getTopLinks: GetTopLinks<TopLinksQuery, TopLinks> = (
  args,
  context,
) =>
  runOperation(() =>
    analytics.getTopLinks(callerPrincipal(context), args ?? {}, deps),
  );
