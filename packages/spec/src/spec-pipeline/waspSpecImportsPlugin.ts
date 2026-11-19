import { createRequire } from "node:module";
import { pathToFileURL } from "node:url";
import type { Plugin } from "rolldown";
import {
  INTERNAL_MAKE_REF_HELPER_IMPORT_SOURCE,
  PUBLIC_REF_HELPER_IMPORT_SOURCE,
} from "./transformWaspTsSpecFilesPlugin/util.js";

const require = createRequire(import.meta.url);

/**
 * Resolves the Wasp Spec imports of the project's spec files to this package.
 *
 * The project imports the Wasp Spec as `wasp/spec`, which only exists as a
 * TypeScript alias, so Node can't resolve it. The same goes for the internal
 * module, which the transform plugin imports into the spec files.
 *
 * The modules stay out of the bundle, so the spec files use the same copy of
 * this package that runs the analysis. The `instanceof` checks on the errors
 * it throws rely on that.
 */
export function waspSpecImportsPlugin(): Plugin {
  const moduleUrlByImportSource: Record<string, string> = {
    [PUBLIC_REF_HELPER_IMPORT_SOURCE]: getOwnModuleUrl("@wasp/spec"),
    [INTERNAL_MAKE_REF_HELPER_IMPORT_SOURCE]: getOwnModuleUrl(
      "@wasp/spec/internal",
    ),
  };

  return {
    name: "wasp:spec-imports",
    resolveId(source) {
      const moduleUrl = moduleUrlByImportSource[source];
      return moduleUrl === undefined ? null : { id: moduleUrl, external: true };
    },
  };
}

/**
 * Node resolves a package's own name from inside the package. The result is a
 * `file:` URL, because the bundler writes it into an import statement as is,
 * and Node rejects a Windows path like `C:\...` there.
 */
function getOwnModuleUrl(ownExport: string): string {
  return pathToFileURL(require.resolve(ownExport)).href;
}
