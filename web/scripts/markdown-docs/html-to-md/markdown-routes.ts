/**
 * Whether a route is (or sits under) one of the Markdown route prefixes.
 */
export function isValidMarkdownDocsRoute(route: string): boolean {
  return (
    route === "/docs" ||
    route.startsWith("/docs/") ||
    route === "/blog" ||
    isBlogPostRoute(route) ||
    route === "/resources" ||
    isResourcesPostRoute(route)
  );
}

function isBlogPostRoute(htmlFileRelPath: string) {
  return /\/blog\/\d{4}-\d{2}-\d{2}/.test(htmlFileRelPath);
}

function isResourcesPostRoute(htmlFileRelPath: string) {
  return /\/resources\/\d{4}-\d{2}-\d{2}/.test(htmlFileRelPath);
}
