/**
 * Whether a HTML file is a valid candidate for generating a
 * markdown variant.
 *
 * HTML file path must be relative to the site dir.
 * E.g. for route "/docs", the file path has to be "docs.html".
 */
export function htmlFileRelPathHasMarkdownVariant(
  htmlFileRelPath: string,
): boolean {
  return routeHasMarkdownVariant(htmlFileRelPathToRoute(htmlFileRelPath));
}

/**
 * Whether a route supports markdown variant of content.
 * Keep in sync with `_routes.json` in `static/` dir.
 */
export function routeHasMarkdownVariant(route: string): boolean {
  return (
    route === "/docs" ||
    route.startsWith("/docs/") ||
    route === "/blog" ||
    isBlogPostRoute(route) ||
    route === "/resources" ||
    isResourcesPostRoute(route)
  );
}

/**
 * Maps a build-relative HTML path to its route.
 *
 * @example "docs/tutorial/create.html" → "/docs/tutorial/create"
 * @example "docs.html" → "/docs"
 * @example "blog/2025/12/31/post.html" → "/blog/2025/12/31/post"
 */
export function htmlFileRelPathToRoute(htmlFileRelPath: string): string {
  return "/" + htmlFileRelPath.replace(/\\/g, "/").replace(/\.html$/, "");
}

function isBlogPostRoute(route: string) {
  return /\/blog\/\d{4}\/\d{2}\/\d{2}/.test(route);
}

function isResourcesPostRoute(route: string) {
  return /\/resources\/\d{4}\/\d{2}\/\d{2}/.test(route);
}
