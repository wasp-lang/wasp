import { existsSync } from "node:fs";
import { isBuiltin } from "node:module";
import * as path from "node:path";
import { fileURLToPath } from "node:url";
import type { Plugin } from "rolldown";
import { OWN_SPEC_ENTRY_URLS } from "../ownSpecEntryPoints.js";

/**
 * Decides, in one place, where every import of the spec files goes:
 * 1. `@wasp.sh/spec` and `@wasp.sh/spec/internal` go to the analyzer's own
 *    entry points (external), never to a copy found in the project.
 * 2. Node builtins are left to rolldown's own `platform: "node"` handling.
 * 3. Bare imports of packages that Node can find from `bundleDir` stay
 *    external and are loaded by Node when the bundle runs. Their names are
 *    recorded in `externalUserlandPackages`.
 * 4. Everything else (relative files, tsconfig path aliases, packages in
 *    nested `node_modules`) gets bundled.
 */
export function resolveSpecImportsPlugin({
  bundleDir,
  externalUserlandPackages,
}: {
  bundleDir: string;
  externalUserlandPackages: Set<string>;
}): Plugin {
  return {
    name: "wasp/spec/resolve-imports",
    resolveId: {
      order: "pre",
      handler(source, _importer, extraOptions) {
        const ownSpecEntryUrl = OWN_SPEC_ENTRY_URLS.get(source);
        if (ownSpecEntryUrl) {
          // `import` needs a URL (portable to Windows), `require()` a path.
          const id =
            extraOptions.kind === "require-call"
              ? fileURLToPath(ownSpecEntryUrl)
              : ownSpecEntryUrl;
          return { id, external: true };
        }

        if (isBuiltin(source)) {
          return null;
        }

        const packageName = getBarePackageName(source);
        if (packageName && isPackageVisibleFrom(bundleDir, packageName)) {
          externalUserlandPackages.add(packageName);
          return { id: source, external: true };
        }

        return null;
      },
    },
  };
}

function getBarePackageName(source: string): string | undefined {
  if (/^[./#\0]/.test(source) || path.isAbsolute(source)) {
    return undefined;
  }
  const [first, second] = source.split("/");
  return source.startsWith("@") ? second && `${first}/${second}` : first;
}

// Mirrors Node's lookup of a bare specifier: `node_modules/<name>` in `dir`
// and in each of its ancestors.
function isPackageVisibleFrom(dir: string, packageName: string): boolean {
  for (let currentDir = dir; ; currentDir = path.dirname(currentDir)) {
    if (existsSync(path.join(currentDir, "node_modules", packageName))) {
      return true;
    }
    if (path.dirname(currentDir) === currentDir) {
      return false;
    }
  }
}
