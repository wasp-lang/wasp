import { existsSync } from "node:fs";
import { createRequire, isBuiltin } from "node:module";
import * as path from "node:path";
import { WASP_SPEC_FILE_REGEX } from "./common.js";

/**
 * Replaces unrun's default external resolver (same rules) with one exception:
 *
 *   main.wasp.ts -> import "@scope/module/spec" -> node_modules/.../module.wasp.ts
 *
 *   external: raw .wasp.ts executed by Node -> ERR_UNSUPPORTED_NODE_MODULES_TYPE_STRIPPING,
 *             `with { type: "ref" }` imports never lowered
 *   bundled:  pipeline plugins transform it like any other spec file
 *
 * Must be the `external` option, not a resolveId plugin: the bundler checks
 * `external` before plugin resolution, and unrun replaces it wholesale
 * without exporting its default.
 */
export function createSpecAwareExternalResolver(specPath: string) {
  const entryDir = path.dirname(specPath);

  return function external(id: string, importer: string | undefined): boolean {
    if (
      !id ||
      id.startsWith("\0") ||
      id.startsWith(".") ||
      id.startsWith("#") ||
      path.isAbsolute(id)
    ) {
      return false;
    }

    if (isBuiltin(id)) {
      return true;
    }

    const resolvedPath = tryResolveFrom(importer ?? specPath, id);
    if (resolvedPath !== undefined && WASP_SPEC_FILE_REGEX.test(resolvedPath)) {
      return false;
    }

    return canResolveFromEntryNodeModules(entryDir, id);
  };
}

function tryResolveFrom(
  importerFilePath: string,
  source: string,
): string | undefined {
  try {
    return createRequire(importerFilePath).resolve(source);
  } catch {
    return undefined;
  }
}

function canResolveFromEntryNodeModules(
  entryDir: string,
  specifier: string,
): boolean {
  const packageName = getPackageName(specifier);
  if (packageName === undefined) {
    return false;
  }

  let currentDir = entryDir;
  while (true) {
    if (existsSync(path.join(currentDir, "node_modules", packageName))) {
      return true;
    }

    const parentDir = path.dirname(currentDir);
    if (parentDir === currentDir) {
      return false;
    }
    currentDir = parentDir;
  }
}

function getPackageName(specifier: string): string | undefined {
  if (specifier.startsWith("@")) {
    const segments = specifier.split("/");
    return segments.length >= 2 ? `${segments[0]}/${segments[1]}` : undefined;
  }

  const [name] = specifier.split("/");
  return name || undefined;
}
