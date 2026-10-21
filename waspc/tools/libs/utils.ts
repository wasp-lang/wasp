import { readFileSync } from "node:fs";
import { basename, join, resolve } from "node:path";
import { discoverSubDirs, getWaspcDirPath } from "../utils.ts";

export function getDataLibsDirPath(): string {
  const waspcDirPath = getWaspcDirPath();
  return join(waspcDirPath, "data", "Generator", "libs");
}

/**
 * The lib dirs ordered so that every lib comes after the libs it depends on
 * through a local `file:` dependency (e.g. `@wasp.sh/lib-auth` after
 * `@wasp.sh/lib-sdk-core`). Building a lib needs its local deps built.
 */
export function discoverLibDirsInDependencyOrder(
  dataLibsDirPath: string,
): string[] {
  const libDirs = discoverSubDirs(dataLibsDirPath);
  const localDepsByLibDir = new Map(
    libDirs.map((libDir) => [libDir, getLocalDependencyDirs(libDir)]),
  );

  const ordered: string[] = [];
  const visiting = new Set<string>();

  function visit(libDir: string): void {
    if (ordered.includes(libDir)) return;
    if (visiting.has(libDir)) {
      throw new Error(
        `Wasp libs depend on each other in a cycle at ${basename(libDir)}`,
      );
    }
    visiting.add(libDir);
    for (const depDir of localDepsByLibDir.get(libDir) ?? []) {
      visit(depDir);
    }
    visiting.delete(libDir);
    ordered.push(libDir);
  }

  for (const libDir of libDirs) {
    visit(libDir);
  }
  return ordered;
}

function getLocalDependencyDirs(libDir: string): string[] {
  const packageJson: {
    dependencies?: Record<string, string>;
    devDependencies?: Record<string, string>;
    peerDependencies?: Record<string, string>;
  } = JSON.parse(readFileSync(join(libDir, "package.json"), "utf-8"));

  // A lib lists a local lib under the same name in `devDependencies` (`file:`)
  // and `peerDependencies` (version range), so the groups are scanned separately.
  const allRanges = [
    packageJson.dependencies,
    packageJson.devDependencies,
    packageJson.peerDependencies,
  ].flatMap((deps) => Object.values(deps ?? {}));
  return allRanges
    .filter((range) => range.startsWith("file:"))
    .map((range) => resolve(libDir, range.slice("file:".length)));
}
