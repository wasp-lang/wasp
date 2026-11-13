// Click analytics. Reads only; clicks are written by redirect.ts.
import type { PrismaClient } from "@prisma/client";
import { dashboardUrls } from "../deeplinks/urls";
import { findOwnedLink, toLinkOut } from "../links/service";
import { requireScope, type Principal } from "../shared/principal";
import { parseInput } from "../shared/validate";
import type { Range } from "./ranges";
import {
  StatsQuery,
  TopLinksQuery,
  type LinkStats,
  type TopLinks,
} from "./schemas";

export type Deps = { prisma: PrismaClient; now?: () => Date };

const DAY_MS = 24 * 60 * 60 * 1000;
const RANGE_MS: Record<Range, number> = {
  "24h": DAY_MS,
  "7d": 7 * DAY_MS,
  "30d": 30 * DAY_MS,
  "90d": 90 * DAY_MS,
};

/**
 * The window a range covers. Multi-day ranges start at UTC midnight so every
 * day bucket is complete (7d = today plus the 6 previous days).
 */
export function rangeWindow(
  range: Range,
  now: Date,
): { since: Date; days: string[] } {
  const since =
    range === "24h"
      ? new Date(now.getTime() - DAY_MS)
      : startOfUtcDay(new Date(now.getTime() - RANGE_MS[range] + DAY_MS));
  const days: string[] = [];
  for (
    let d = startOfUtcDay(since);
    d <= now;
    d = new Date(d.getTime() + DAY_MS)
  )
    days.push(dayKey(d));
  return { since, days };
}

export async function getLinkStats(
  p: Principal,
  query: StatsQuery,
  { prisma, now = () => new Date() }: Deps,
): Promise<LinkStats> {
  requireScope(p, "analytics:read");
  const q = parseInput(StatsQuery, query);
  const link = await findOwnedLink(prisma, p.userId, q.linkId);
  const { since, days } = rangeWindow(q.range, now());

  let series: { key: string; clicks: number }[];
  if (q.groupBy === "day") {
    const rows = await prisma.$queryRaw<{ day: Date; clicks: number }[]>`
      SELECT date_trunc('day', "createdAt" AT TIME ZONE 'UTC') AS day, count(*)::int AS clicks
      FROM "Click"
      WHERE "linkId" = ${link.id} AND "createdAt" >= ${since}
      GROUP BY 1`;
    const byDay = new Map(rows.map((r) => [dayKey(new Date(r.day)), r.clicks]));
    // Zero-fill so charts have no gaps.
    series = days.map((key) => ({ key, clicks: byDay.get(key) ?? 0 }));
  } else {
    const rows = await prisma.click.groupBy({
      by: ["referrer"],
      where: { linkId: link.id, createdAt: { gte: since } },
      _count: { _all: true },
    });
    series = rows
      .map((r) => ({ key: r.referrer ?? "direct", clicks: r._count._all }))
      .sort((a, b) => b.clicks - a.clicks || a.key.localeCompare(b.key));
  }

  return {
    linkId: link.id,
    slug: link.slug,
    range: q.range,
    groupBy: q.groupBy,
    total: series.reduce((sum, s) => sum + s.clicks, 0),
    series,
    dashboardUrl: dashboardUrls.link(link.id, {
      tab: "stats",
      range: q.range,
      groupBy: q.groupBy,
    }),
  };
}

export async function getTopLinks(
  p: Principal,
  query: TopLinksQuery,
  { prisma, now = () => new Date() }: Deps,
): Promise<TopLinks> {
  requireScope(p, "analytics:read");
  const q = parseInput(TopLinksQuery, query);
  const { since } = rangeWindow(q.range, now());

  const counts = await prisma.click.groupBy({
    by: ["linkId"],
    where: { createdAt: { gte: since }, link: { userId: p.userId } },
    _count: { _all: true },
    orderBy: { _count: { linkId: "desc" } },
    take: q.limit,
  });
  const links = await prisma.link.findMany({
    where: { id: { in: counts.map((c) => c.linkId) }, userId: p.userId },
  });
  const byId = new Map(links.map((l) => [l.id, l]));

  return {
    range: q.range,
    data: counts.flatMap((c) => {
      const link = byId.get(c.linkId);
      return link ? [{ link: toLinkOut(link), clicks: c._count._all }] : [];
    }),
    dashboardUrl: dashboardUrls.topLinks({ range: q.range }),
  };
}

function startOfUtcDay(d: Date): Date {
  return new Date(
    Date.UTC(d.getUTCFullYear(), d.getUTCMonth(), d.getUTCDate()),
  );
}

function dayKey(d: Date): string {
  return d.toISOString().slice(0, 10);
}
