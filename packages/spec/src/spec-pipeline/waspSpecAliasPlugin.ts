import type { Plugin } from "rolldown";

/**
 * Resolves `@wasp/spec` imports in the user's spec files to the copy of
 * `@wasp/spec` that runs the analysis.
 *
 * The analyzer checks the errors the spec throws with `instanceof`, which only
 * works when the spec and the analyzer share one copy of the package. npm
 * hoists `@wasp/spec` next to `@wasp/wasp`, but this plugin doesn't rely on
 * that. It also keeps the bundler from looking for `@wasp/spec` at the project
 * root, where package managers that don't hoist (pnpm) don't put it.
 *
 * The plugin marks the modules external, so the user's spec imports them at
 * runtime instead of bundling a second copy. The IDs are `file:` URLs, not
 * paths. The bundler writes them verbatim into the output's import statement.
 * Node accepts a `file:` URL on every platform, but rejects a Windows path
 * such as `C:\...` as an unsupported URL scheme.
 */
export function waspSpecAliasPlugin(): Plugin {
  const specEntryPoints: Record<string, string> = {
    "@wasp/spec": getSpecDistFileUrl("index.js"),
    "@wasp/spec/internal": getSpecDistFileUrl("internal.js"),
  };

  return {
    name: "wasp-spec-alias",
    resolveId(source) {
      const specEntryPoint = specEntryPoints[source];
      return specEntryPoint === undefined
        ? null
        : { id: specEntryPoint, external: true };
    },
  };
}

// This file lives in `dist/src/spec-pipeline/`, the entry points in `dist/src/`.
function getSpecDistFileUrl(fileName: string): string {
  return new URL(`../${fileName}`, import.meta.url).href;
}
