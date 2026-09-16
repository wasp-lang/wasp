/**
 * Wasp's own auth records each method's identities in its own namespace
 * under the scheme's name: `<scheme>:username`, `<scheme>:email`,
 * `<scheme>:google`, ... The manifest (see `spec.ts`) declares exactly
 * these suffixes, and the scheme name comes from the app's `auth.schemes`.
 */
export function namespaceFor(runtime, method) {
    return `${runtime.scheme}:${method}`;
}
