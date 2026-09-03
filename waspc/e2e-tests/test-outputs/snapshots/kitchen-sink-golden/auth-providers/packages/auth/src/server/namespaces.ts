/**
 * Wasp's own auth is the `wasp` provider and records each method's identities
 * in its own namespace under it: `wasp:username`, `wasp:email`, `wasp:google`,
 * ... The manifest (see `spec.ts`) declares exactly these.
 */
export const PROVIDER_ID = "wasp";

export const DEFAULT_ROUTES_BASE_PATH = "/auth/wasp";

export function namespaceFor(method: string): string {
  return `${PROVIDER_ID}:${method}`;
}
