import { describe, expect, it } from "vitest";
import { SsrRoutes } from "../src/plugins/common/routes";

const SPA_FALLBACK_FILE = "200.html";

describe("SsrRoutes", () => {
  describe("route ids", () => {
    it("maps paths to directory-style html files", () => {
      const routes = new SsrRoutes(["/", "/about"], SPA_FALLBACK_FILE);

      expect(routes.getAllIds()).toEqual([
        "index.html",
        "about/index.html",
        "200.html",
      ]);
    });
  });

  describe("match", () => {
    const routes = new SsrRoutes(
      ["/", "/about", "/docs/intro"],
      SPA_FALLBACK_FILE,
    );

    it("matches an exact route path", () => {
      expect(routes.match("/about", "/").path).toBe("/about");
      expect(routes.match("/docs/intro", "/").path).toBe("/docs/intro");
    });

    it("matches the root path", () => {
      expect(routes.match("/", "/").path).toBe("/");
    });

    it("falls back to the SPA fallback for unknown paths", () => {
      expect(routes.match("/faq", "/")).toBe(routes.spaFallbackFile);
      expect(routes.match("/about/team", "/")).toBe(routes.spaFallbackFile);
    });

    it("ignores query strings and hashes", () => {
      expect(routes.match("/about?tab=1", "/").path).toBe("/about");
      expect(routes.match("/about#team", "/").path).toBe("/about");
      expect(routes.match("/?utm_source=x", "/").path).toBe("/");
    });

    it("ignores a trailing slash", () => {
      expect(routes.match("/about/", "/").path).toBe("/about");
      expect(routes.match("/about/?tab=1", "/").path).toBe("/about");
    });

    it("matches percent-encoded paths", () => {
      expect(routes.match("/ab%6Fut", "/").path).toBe("/about");
    });

    it("falls back to the SPA fallback for malformed percent-encoding", () => {
      expect(routes.match("/%zz", "/")).toBe(routes.spaFallbackFile);
    });

    it("matches routes configured with a trailing slash", () => {
      const routesWithTrailingSlash = new SsrRoutes(
        ["/docs/"],
        SPA_FALLBACK_FILE,
      );

      expect(routesWithTrailingSlash.match("/docs", "/").path).toBe("/docs/");
      expect(routesWithTrailingSlash.match("/docs/", "/").path).toBe("/docs/");
    });

    describe("with a non-root base", () => {
      it("strips the base prefix before matching", () => {
        expect(routes.match("/app/about", "/app/").path).toBe("/about");
        expect(routes.match("/app/about/", "/app/").path).toBe("/about");
        expect(routes.match("/app/about?tab=1", "/app/").path).toBe("/about");
      });

      it("matches the root path at the base, with or without its trailing slash", () => {
        expect(routes.match("/app/", "/app/").path).toBe("/");
        expect(routes.match("/app", "/app/").path).toBe("/");
      });

      it("falls back to the SPA fallback for paths outside the base", () => {
        expect(routes.match("/about", "/app/")).toBe(routes.spaFallbackFile);
        expect(routes.match("/apple/about", "/app/")).toBe(
          routes.spaFallbackFile,
        );
      });
    });
  });
});
