// All absolute URLs are built from these two bases. No hardcoded hosts
// anywhere else. The Wasp server's setupFn calls configureUrls() with Wasp's
// `config` values; the env-var defaults keep framework-free code and tests
// working without Wasp.

// `process` doesn't exist in the browser bundle (the dashboard imports the
// deep-link builders but only uses their relative paths).
const env: Record<string, string | undefined> =
  typeof process !== "undefined" ? process.env : {};

let bases = {
  serverUrl: stripSlash(env.WASP_SERVER_URL ?? "http://localhost:3001"),
  frontendUrl: stripSlash(env.WASP_WEB_CLIENT_URL ?? "http://localhost:3000"),
};

export function configureUrls(next: {
  serverUrl: string;
  frontendUrl: string;
}): void {
  bases = {
    serverUrl: stripSlash(next.serverUrl),
    frontendUrl: stripSlash(next.frontendUrl),
  };
}

export const serverUrl = (): string => bases.serverUrl;
export const frontendUrl = (): string => bases.frontendUrl;
export const mcpUrl = (): string => `${bases.serverUrl}/mcp`;
export const shortUrl = (slug: string): string =>
  `${bases.serverUrl}/r/${slug}`;

function stripSlash(url: string): string {
  return url.replace(/\/+$/, "");
}
