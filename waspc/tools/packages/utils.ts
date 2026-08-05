import { join } from "node:path";
import { discoverSubDirs, getWaspcDirPath } from "../utils.ts";

// `studio-server` serves `studio-client`'s build output (its `public` dir is a
// symlink into `studio-client/dist`), so the client has to be built first.
// Everything else is independent, so plain alphabetical order is fine.
const PACKAGES_TO_BUILD_FIRST = ["studio-client"];

export function getPackagesDirPath(): string {
  const waspcDirPath = getWaspcDirPath();
  return join(waspcDirPath, "packages");
}

/**
 * Returns the package dirs in the order they must be built in.
 */
export function discoverPackageDirs(): string[] {
  const packageDirs = discoverSubDirs(getPackagesDirPath());
  return packageDirs.sort(
    (a, b) => buildPriority(a) - buildPriority(b) || a.localeCompare(b),
  );

  function buildPriority(packageDir: string): number {
    const index = PACKAGES_TO_BUILD_FIRST.findIndex((packageDirName) =>
      packageDir.endsWith(packageDirName),
    );
    return index === -1 ? PACKAGES_TO_BUILD_FIRST.length : index;
  }
}
