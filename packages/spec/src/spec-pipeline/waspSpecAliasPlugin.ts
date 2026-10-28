import { fileURLToPath } from "node:url";
import type { Plugin } from "rolldown";
import {
  INTERNAL_MAKE_REF_HELPER_IMPORT_SOURCE,
  PUBLIC_REF_HELPER_IMPORT_SOURCE,
} from "./transformWaspTsSpecFilesPlugin/util.js";

/**
 * Resolves imports of this package (`@wasp/spec`) in the user's spec files to
 * the copy of the package that is running the analysis.
 *
 * Users don't depend on `@wasp/spec` directly, they depend on `@wasp/wasp`,
 * which depends on it. Package managers that don't hoist (e.g. pnpm) then
 * don't expose `@wasp/spec` at the project root, so the bundler couldn't
 * resolve it from `main.wasp.ts`. Pointing it at our own copy also guarantees
 * there is a single copy of the package at runtime, which matters for
 * `instanceof` checks on the errors it throws.
 *
 * The resolved modules are marked external, so the user's spec file keeps
 * importing them at runtime instead of bundling a second copy.
 */
export function waspSpecAliasPlugin(): Plugin {
  const specEntryPoints: Record<string, string> = {
    [PUBLIC_REF_HELPER_IMPORT_SOURCE]: getSpecDistPath("index.js"),
    [INTERNAL_MAKE_REF_HELPER_IMPORT_SOURCE]: getSpecDistPath("internal.js"),
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
function getSpecDistPath(fileName: string): string {
  return fileURLToPath(new URL(`../${fileName}`, import.meta.url));
}
