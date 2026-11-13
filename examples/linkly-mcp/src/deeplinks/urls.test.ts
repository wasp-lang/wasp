import { readdirSync, readFileSync } from "node:fs";
import { join } from "node:path";
import { describe, expect, it } from "vitest";
import {
  DASHBOARD_ROUTE_PATTERNS,
  dashboardPaths,
  dashboardUrls,
} from "./urls";

function declaredRoutes(): string[] {
  const routes: string[] = [];
  const walk = (dir: string) => {
    for (const entry of readdirSync(dir, { withFileTypes: true })) {
      const path = join(dir, entry.name);
      if (entry.isDirectory()) walk(path);
      else if (entry.name.endsWith(".wasp.ts")) {
        for (const m of readFileSync(path, "utf8").matchAll(
          /route\(\s*"\w+",\s*"([^"]+)"/g,
        ))
          routes.push(m[1]!);
      }
    }
  };
  walk(join(process.cwd(), "src"));
  return routes;
}

const toRegex = (pattern: string) =>
  new RegExp(`^${pattern.replace(/:\w+/g, "[^/?]+")}(\\?.*)?$`);

describe("dashboard URL builders", () => {
  it("target routes that exist in the *.wasp.ts files", () => {
    const routes = declaredRoutes();
    for (const [builder, pattern] of Object.entries(DASHBOARD_ROUTE_PATTERNS)) {
      expect(routes, `${builder} → ${pattern}`).toContain(pattern);
    }
  });

  it("produce paths matching their route pattern", () => {
    const samples: Record<keyof typeof dashboardPaths, string> = {
      links: dashboardPaths.links({ search: "sale", tag: "promo" }),
      link: dashboardPaths.link("abc", {
        tab: "stats",
        range: "30d",
        groupBy: "referrer",
      }),
      newLink: dashboardPaths.newLink({
        destinationUrl: "https://a.com/x?y=1",
        tags: ["a", "b"],
      }),
      topLinks: dashboardPaths.topLinks({ range: "24h" }),
      connections: dashboardPaths.connections(),
      doc: dashboardPaths.doc("tags"),
    };
    for (const [builder, path] of Object.entries(samples)) {
      expect(path, builder).toMatch(
        toRegex(DASHBOARD_ROUTE_PATTERNS[builder as keyof typeof samples]),
      );
    }
  });

  it("encode parameters the pages read", () => {
    expect(
      dashboardPaths.link("abc", {
        tab: "stats",
        range: "30d",
        groupBy: "referrer",
      }),
    ).toBe("/links/abc?tab=stats&range=30d&groupBy=referrer");
    expect(
      dashboardPaths.newLink({
        destinationUrl: "https://a.com/x?y=1",
        tags: ["a", "b"],
      }),
    ).toBe(
      "/links/new?destinationUrl=https%3A%2F%2Fa.com%2Fx%3Fy%3D1&tags=a%2Cb",
    );
    expect(dashboardUrls.topLinks({ range: "24h" })).toBe(
      "http://localhost:3000/analytics?range=24h",
    );
  });
});
