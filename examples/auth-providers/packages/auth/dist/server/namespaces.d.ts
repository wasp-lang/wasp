/**
 * Wasp's own auth is the `wasp` provider and records each method's identities
 * in its own namespace under it: `wasp:username`, `wasp:email`, `wasp:google`,
 * ... The manifest (see `spec.ts`) declares exactly these.
 */
export declare const PROVIDER_ID = "wasp";
export declare const DEFAULT_ROUTES_BASE_PATH = "/auth/wasp";
export declare function namespaceFor(method: string): string;
