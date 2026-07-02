import * as posixPath from "node:path/posix";
import {
  getRoutablePathname,
  removeLeadingSlash,
  removeTrailingSlash,
} from "../util/path";

export interface SsrRoute {
  /** The pathname visible in the browser's address bar. */
  path: string;

  /** Vite's Module ID for the route */
  id: string;
}

/**
 * This class holds the collection of routes to be SSR'd, and precomputes lookup
 * maps for them to quickly find a route while in the middle of a request or a
 * build.
 */
export class SsrRoutes {
  public readonly byId: ReadonlyMap<string, Readonly<SsrRoute>>;
  public readonly byPath: ReadonlyMap<string, Readonly<SsrRoute>>;
  public readonly spaFallbackFile: Readonly<SsrRoute>;

  constructor(paths: readonly string[], spaFallbackFile: string) {
    // SPA fallback gets a flat file (e.g. "200.html") instead of the
    // directory-style "dir/index.html" used for real routes, since it's
    // never accessed by path — only served as the SPA catch-all.
    this.spaFallbackFile = {
      path: spaFallbackFile,
      id: removeLeadingSlash(spaFallbackFile),
    };

    const routes = [
      ...paths.map((path) => ({
        path,
        // Note: for "/" this produces "index.html" (posixPath.join("", "index.html")).
        id: posixPath.join(removeLeadingSlash(path), "index.html"),
      })),
      this.spaFallbackFile,
    ];

    this.byId = new Map(routes.map((route) => [route.id, route]));
    this.byPath = new Map(
      routes.map((route) => [removeTrailingSlash(route.path), route]),
    );
  }

  getAllIds() {
    return Array.from(this.byId.keys());
  }

  /**
   * Finds the route matching a request URL, falling back to the SPA fallback
   * when nothing matches. Unlike the configured route paths, the request URL
   * may contain a query string, percent-encoding, a trailing slash, or Vite's
   * `base` prefix, so it's normalized before lookup.
   */
  match(requestUrl: string, base: string): Readonly<SsrRoute> {
    const pathname = getRoutablePathname(requestUrl, base);
    if (pathname === null) {
      return this.spaFallbackFile;
    }
    return this.byPath.get(pathname) ?? this.spaFallbackFile;
  }
}
