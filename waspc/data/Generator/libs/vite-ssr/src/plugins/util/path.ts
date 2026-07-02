import * as fs from "node:fs/promises";

export function removeLeadingSlash(path: string) {
  return path.replace(/^\//, "");
}

/** Removes a single trailing slash, keeping the root path ("/") intact. */
export function removeTrailingSlash(path: string) {
  return path.length > 1 && path.endsWith("/") ? path.slice(0, -1) : path;
}

/**
 * Extracts the app-relative pathname from a raw request URL so it can be
 * compared against configured route paths: drops the query string and hash,
 * percent-decodes it, removes Vite's `base` prefix, and normalizes the
 * trailing slash.
 *
 * Returns `null` when the URL can't be mapped to a route pathname (malformed
 * percent-encoding, or a URL outside `base`).
 */
export function getRoutablePathname(
  requestUrl: string,
  base: string,
): string | null {
  let pathname: string;
  try {
    // The dummy origin lets `URL` parse the origin-relative URLs that
    // middlewares receive (e.g. "/about?tab=1").
    pathname = decodeURIComponent(
      new URL(requestUrl, "http://localhost").pathname,
    );
  } catch {
    return null;
  }

  const pathnameWithoutBase = stripBase(pathname, base);
  if (pathnameWithoutBase === null) {
    return null;
  }

  return removeTrailingSlash(pathnameWithoutBase);
}

/**
 * Removes Vite's `base` prefix from a decoded pathname, returning `null` for
 * pathnames outside `base`. A resolved Vite base is "/" or a path with
 * leading and trailing slashes (e.g. "/app/"); other forms (a full URL or
 * the relative "./") never match a request pathname and yield `null`.
 */
function stripBase(pathname: string, base: string): string | null {
  if (base === "/") {
    return pathname;
  }

  const baseWithoutTrailingSlash = removeTrailingSlash(base);
  if (pathname === baseWithoutTrailingSlash) {
    return "/";
  }
  if (pathname.startsWith(baseWithoutTrailingSlash + "/")) {
    return pathname.slice(baseWithoutTrailingSlash.length);
  }
  return null;
}

export async function pathExists(...params: Parameters<typeof fs.access>) {
  try {
    await fs.access(...params);
    return true;
  } catch {
    return false;
  }
}
