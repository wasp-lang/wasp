/**
 * Routes that get a generated `.md` counterpart.
 */
const MARKDOWN_ROUTE_PREFIXES = ["/docs", "/blog", "/resources"];

/** Whether a route is (or sits under) one of the Markdown route prefixes. */
export function isMarkdownRoute(pathname: string): boolean {
  return MARKDOWN_ROUTE_PREFIXES.some(
    (prefix) => pathname === prefix || pathname.startsWith(prefix + "/"),
  );
}
