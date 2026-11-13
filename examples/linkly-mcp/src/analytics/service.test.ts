import { beforeEach, describe, expect, it } from "vitest";
import { createLink } from "../links/service";
import { DomainError } from "../shared/errors";
import {
  principalFromMcp,
  principalFromSession,
  type Principal,
} from "../shared/principal";
import { createTestUser, prisma, resetDb } from "../test/db";
import { recordClick, referrerHost, resolveRedirect } from "./clicks";
import { getLinkStats, getTopLinks, rangeWindow } from "./service";

const NOW = new Date("2026-09-25T15:00:00Z");
const deps = { prisma, now: () => NOW };
let alice: Principal;
let bob: Principal;

beforeEach(async () => {
  await resetDb();
  alice = principalFromSession(await createTestUser());
  bob = principalFromSession(await createTestUser());
});

async function clickAt(
  linkId: string,
  iso: string,
  referrer: string | null = null,
) {
  await prisma.click.create({
    data: { linkId, createdAt: new Date(iso), referrer },
  });
}

describe("rangeWindow", () => {
  it("covers today plus the previous 6 days for 7d", () => {
    const { since, days } = rangeWindow("7d", NOW);
    expect(since.toISOString()).toBe("2026-09-19T00:00:00.000Z");
    expect(days).toEqual([
      "2026-09-19",
      "2026-09-20",
      "2026-09-21",
      "2026-09-22",
      "2026-09-23",
      "2026-09-24",
      "2026-09-25",
    ]);
  });

  it("covers the last 24 hours exactly for 24h", () => {
    const { since, days } = rangeWindow("24h", NOW);
    expect(since.toISOString()).toBe("2026-09-24T15:00:00.000Z");
    expect(days).toEqual(["2026-09-24", "2026-09-25"]);
  });

  it("has 90 days for 90d", () => {
    expect(rangeWindow("90d", NOW).days).toHaveLength(90);
  });
});

describe("getLinkStats", () => {
  it("zero-fills days with no clicks", async () => {
    const link = await createLink(
      alice,
      { destinationUrl: "https://a.com", slug: "stats" },
      deps,
    );
    await clickAt(link.id, "2026-09-20T10:00:00Z");
    await clickAt(link.id, "2026-09-20T23:59:00Z");
    await clickAt(link.id, "2026-09-25T01:00:00Z");
    await clickAt(link.id, "2026-09-10T01:00:00Z"); // outside 7d

    const stats = await getLinkStats(alice, { linkId: "stats" }, deps);
    expect(stats.total).toBe(3);
    expect(stats.series).toEqual([
      { key: "2026-09-19", clicks: 0 },
      { key: "2026-09-20", clicks: 2 },
      { key: "2026-09-21", clicks: 0 },
      { key: "2026-09-22", clicks: 0 },
      { key: "2026-09-23", clicks: 0 },
      { key: "2026-09-24", clicks: 0 },
      { key: "2026-09-25", clicks: 1 },
    ]);
    expect(stats.dashboardUrl).toBe(
      `http://localhost:3000/links/${link.id}?tab=stats&range=7d&groupBy=day`,
    );
  });

  it("groups by referrer, with direct for missing referrers", async () => {
    const link = await createLink(
      alice,
      { destinationUrl: "https://a.com" },
      deps,
    );
    await clickAt(link.id, "2026-09-25T01:00:00Z", "news.ycombinator.com");
    await clickAt(link.id, "2026-09-25T02:00:00Z", "news.ycombinator.com");
    await clickAt(link.id, "2026-09-25T03:00:00Z", null);

    const stats = await getLinkStats(
      alice,
      { linkId: link.id, groupBy: "referrer", range: "30d" },
      deps,
    );
    expect(stats.series).toEqual([
      { key: "news.ycombinator.com", clicks: 2 },
      { key: "direct", clicks: 1 },
    ]);
  });

  it("enforces ownership and the analytics:read scope", async () => {
    const link = await createLink(
      alice,
      { destinationUrl: "https://a.com" },
      deps,
    );
    await expect(
      getLinkStats(bob, { linkId: link.id }, deps),
    ).rejects.toSatisfy(
      (e: unknown) => e instanceof DomainError && e.code === "not_found",
    );
    const linksOnly = principalFromMcp({
      user: { id: alice.userId },
      scopes: ["links:read"],
      connection: { clientName: "Claude" },
    });
    await expect(
      getLinkStats(linksOnly, { linkId: link.id }, deps),
    ).rejects.toSatisfy(
      (e: unknown) => e instanceof DomainError && e.code === "forbidden",
    );
  });

  it("rejects unknown ranges", async () => {
    const link = await createLink(
      alice,
      { destinationUrl: "https://a.com" },
      deps,
    );
    await expect(
      getLinkStats(alice, { linkId: link.id, range: "1y" as never }, deps),
    ).rejects.toSatisfy(
      (e: unknown) => e instanceof DomainError && e.code === "invalid",
    );
  });
});

describe("getTopLinks", () => {
  it("ranks the user's links by clicks in the range", async () => {
    const a = await createLink(
      alice,
      { destinationUrl: "https://a.com", slug: "aaa" },
      deps,
    );
    const b = await createLink(
      alice,
      { destinationUrl: "https://b.com", slug: "bbb" },
      deps,
    );
    await createLink(
      alice,
      { destinationUrl: "https://c.com", slug: "ccc" },
      deps,
    );
    const other = await createLink(
      bob,
      { destinationUrl: "https://bob.com" },
      deps,
    );
    for (let i = 0; i < 3; i++) await clickAt(b.id, "2026-09-24T10:00:00Z");
    await clickAt(a.id, "2026-09-24T10:00:00Z");
    await clickAt(other.id, "2026-09-24T10:00:00Z");

    const top = await getTopLinks(alice, { range: "7d" }, deps);
    expect(top.data.map((d) => [d.link.slug, d.clicks])).toEqual([
      ["bbb", 3],
      ["aaa", 1],
    ]);
    expect(top.dashboardUrl).toBe("http://localhost:3000/analytics?range=7d");
  });
});

describe("redirect helpers", () => {
  it("resolves active links and hides archived ones", async () => {
    const link = await createLink(
      alice,
      { destinationUrl: "https://a.com/x", slug: "go-go" },
      deps,
    );
    expect(await resolveRedirect(prisma, "go-go")).toEqual({
      linkId: link.id,
      destinationUrl: "https://a.com/x",
    });
    await prisma.link.update({
      where: { id: link.id },
      data: { archived: true },
    });
    expect(await resolveRedirect(prisma, "go-go")).toBeNull();
    expect(await resolveRedirect(prisma, "missing")).toBeNull();
  });

  it("stores only the referrer hostname and a truncated user agent", async () => {
    const link = await createLink(
      alice,
      { destinationUrl: "https://a.com" },
      deps,
    );
    await recordClick(prisma, link.id, {
      referer: "https://Twitter.com/some/path?q=1",
      userAgent: "x".repeat(500),
    });
    const click = await prisma.click.findFirstOrThrow({
      where: { linkId: link.id },
    });
    expect(click.referrer).toBe("twitter.com");
    expect(click.userAgent).toHaveLength(200);
    expect(referrerHost("not a url")).toBeNull();
  });
});
