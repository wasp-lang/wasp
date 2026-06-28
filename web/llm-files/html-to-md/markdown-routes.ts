/**
 * Whether a HTML file is a valid candidate for generating a
 * markdown varaint.
 *
 * HTML file path must be relative to the build dir.
 */
export function isHtmlFileAValidMarkdownDocsCandidate(
  htmlFileRelPath: string,
): boolean {
  return isValidMarkdownDocsRoute(htmlFileRelPathToRoute(htmlFileRelPath));
}

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

/**
 * Maps a build-relative HTML path to its route.
 *
 * @example "docs/tutorial/create.html" → "/docs/tutorial/create"
 * @example "docs.html" → "/docs"
 * @example "blog/2025/12/31/post.html" → "/blog/2025-12-31/post"
 */
function htmlFileRelPathToRoute(htmlFileRelPath: string): string {
  return (
    "/" +
    htmlFileRelPath
      .replace(/\\/g, "/")
      .replace(/\.html$/, "")
      // "YYYY/MM/DD" → "YYYY-MM-DD"
      .replace(/(\d{4})\/(\d{2})\/(\d{2})/g, "$1-$2-$3")
  );
}

function isBlogPostRoute(htmlFileRelPath: string) {
  return /\/blog\/\d{4}-\d{2}-\d{2}/.test(htmlFileRelPath);
}

function isResourcesPostRoute(htmlFileRelPath: string) {
  return /\/resources\/\d{4}-\d{2}-\d{2}/.test(htmlFileRelPath);
}
