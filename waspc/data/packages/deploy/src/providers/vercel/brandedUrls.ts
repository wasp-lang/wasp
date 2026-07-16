/**
 * Deterministic Vercel project names and production URLs derived from the
 * Wasp app name. Knowing both URLs before either deploy runs is what solves
 * the client<->server chicken-and-egg problem (the client bundle needs the
 * server URL at build time, the server needs the client origin for CORS) -
 * same trick as the Railway provider.
 */

// Vercel project names: lowercase alphanumerics plus ".", "_", "-", up to
// 100 chars. We reserve room for the longest suffix we append ("-server").
const VALID_APP_NAME_RE = /^[a-z0-9][a-z0-9._-]*$/;
const MAX_APP_NAME_LENGTH = 90;

export function getServerProjectName(appName: string): string {
  return `${appName}-server`;
}

export function getClientProjectName(appName: string): string {
  return `${appName}-client`;
}

export function getServerAppUrl(appName: string): string {
  return `https://${getServerProjectName(appName)}.vercel.app`;
}

export function getClientAppUrl(appName: string): string {
  return `https://${getClientProjectName(appName)}.vercel.app`;
}

export function assertVercelAppNameIsValid(appName: string): void {
  if (!VALID_APP_NAME_RE.test(appName)) {
    throw new Error(
      [
        `Invalid app name: "${appName}".`,
        "Vercel project names must start with a lowercase letter or digit and",
        'may only contain lowercase letters, digits, ".", "_" and "-".',
      ].join(" "),
    );
  }
  if (appName.length > MAX_APP_NAME_LENGTH) {
    throw new Error(
      `App name too long: must be at most ${MAX_APP_NAME_LENGTH} characters ` +
        `(so the "-server"/"-client" suffixes fit Vercel's limit).`,
    );
  }
}
