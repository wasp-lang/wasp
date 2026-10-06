/**
 * `file:` URLs of this package's own `@wasp.sh/spec` entry points.
 *
 * The analyzer runs from the Wasp CLI's installation, not from the user's
 * project, so every import of `@wasp.sh/spec` made while analyzing a spec must
 * land here instead of on a copy in the project's `node_modules` (which may be
 * missing or belong to another Wasp version). Using the same files as the
 * analyzer also keeps a single module instance, which `instanceof` checks like
 * the one for `WaspSpecUserError` rely on.
 */
export const OWN_SPEC_ENTRY_URLS: ReadonlyMap<string, string> = new Map([
  ["@wasp.sh/spec", new URL("./index.js", import.meta.url).href],
  ["@wasp.sh/spec/internal", new URL("./internal.js", import.meta.url).href],
]);
