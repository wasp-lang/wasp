import { existsSync } from "node:fs";
import { createRequire, isBuiltin } from "node:module";
import * as path from "node:path";
import { WASP_SPEC_FILE_REGEX } from "./common.js";

/**
 * Decides which imports stay external to the spec bundle. Mirrors unrun's
 * default resolver (builtins and bare imports resolvable from the entry's
 * `node_modules` stay external) with one exception: bare imports that resolve
 * to `*.wasp.ts` files are bundled.
 *
 * Wasp spec files are a Wasp dialect, not plain TS: only the Wasp pipeline may
 * transform them. Package spec exports (e.g. `@scope/module/spec`) resolve to
 * `*.wasp.ts` sources, so they must go through the pipeline's transform
 * plugins instead of being executed as-is.
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
