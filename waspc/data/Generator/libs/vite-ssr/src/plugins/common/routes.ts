import * as posixPath from "node:path/posix";

/**
 * This are all different ways of referring to the same underlying route.
 */
export interface Route {
  /** The pathname visible in the browser's address bar. */
  path: string;

  /** Vite's Module ID for the route */
  id: string;
}

export class Routes {
  public readonly byId: ReadonlyMap<string, Readonly<Route>>;
  public readonly byPath: ReadonlyMap<string, Readonly<Route>>;
  public readonly fallback: Readonly<Route>;

  constructor(paths: readonly string[], fallbackFile: string) {
    // Fallback gets a flat file (e.g. "/_fallback.html") instead of the
    // directory-style "dir/index.html" used for real routes, since it's
    // never accessed by path — only served as the SPA catch-all.
    this.fallback = {
      path: fallbackFile,
      id: removeLeadingSlash(fallbackFile),
    };

    const routes = [
      ...paths.map((path) => ({
        path,
        // Note: for "/" this produces "index.html" (posixPath.join("", "index.html")).
        id: posixPath.join(removeLeadingSlash(path), "index.html"),
      })),
      this.fallback,
    ];

    this.byId = new Map(routes.map((route) => [route.id, route]));
    this.byPath = new Map(routes.map((route) => [route.path, route]));
  }
}

function removeLeadingSlash(path: string) {
  return path.replace(/^\//, "");
}
