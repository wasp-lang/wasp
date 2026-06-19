/**
 * Whether a route is (or sits under) one of the Markdown route prefixes.
 */
export function isValidMarkdownDocsRoute(pathname: string): boolean {
  return (
    pathname === "/docs" ||
    pathname.startsWith("/docs/") ||
    isBlogPostPathname(pathname)
  );
}

function isBlogPostPathname(pathname: string) {
  return /\/blog\/\d{4}-\d{2}-\d{2}/.test(pathname);
}
