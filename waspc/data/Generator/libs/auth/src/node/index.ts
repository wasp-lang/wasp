export { TimeSpan, createJWTHelpers } from "./jwt";

export { hashPassword, verifyPassword } from "./password";

/**
 * Parses a cookie header string into a Map of cookie name-value pairs.
 * Replacement for oslo/cookie's parseCookies function.
 *
 * @param header The raw Cookie header string (e.g. "name1=value1; name2=value2").
 * @returns A Map of cookie names to their decoded values.
 */
export function parseCookies(header: string): Map<string, string> {
  const map = new Map<string, string>();
  if (!header) return map;
  for (const pair of header.split("; ")) {
    const idx = pair.indexOf("=");
    if (idx === -1) continue;
    const name = pair.slice(0, idx).trim();
    const value = pair.slice(idx + 1);
    try {
      map.set(name, decodeURIComponent(value));
    } catch {
      map.set(name, value);
    }
  }
  return map;
}
