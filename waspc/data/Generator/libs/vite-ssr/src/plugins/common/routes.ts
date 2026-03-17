import * as posixPath from "node:path/posix";
import { removeLeadingSlash } from "../util/path";

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
  public readonly fallbackFile: Readonly<SsrRoute>;

  constructor(paths: readonly string[], fallbackFile: string) {
    // Fallback gets a flat file (e.g. "/_fallback.html") instead of the
    // directory-style "dir/index.html" used for real routes, since it's
    // never accessed by path — only served as the SPA catch-all.
    this.fallbackFile = {
      path: fallbackFile,
      id: removeLeadingSlash(fallbackFile),
    };

    const routes = [
      ...paths.map((path) => ({
        path,
        // Note: for "/" this produces "index.html" (posixPath.join("", "index.html")).
        id: posixPath.join(removeLeadingSlash(path), "index.html"),
      })),
      this.fallbackFile,
    ];

    this.byId = new Map(routes.map((route) => [route.id, route]));
    this.byPath = new Map(routes.map((route) => [route.path, route]));
  }

  getAllIds() {
    return Array.from(this.byId.keys());
  }
}
