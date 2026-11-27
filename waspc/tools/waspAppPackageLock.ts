import { existsSync } from "node:fs";
import { readFile, writeFile } from "node:fs/promises";
import { join } from "node:path";
import { WASP_PACKAGE_NAMES } from "./packages/utils.ts";

// TODO(#2262): Drop this file once the repo's Wasp apps join the pnpm workspace.

/**
 * The repo's Wasp apps install the dev Wasp npm packages from `file:` tarballs
 * (see `mise-tasks/build/dev-packages.ts`), which go stale whenever they're
 * repacked.
 */
export async function removeStaleWaspLockEntries(
  waspAppDir: string,
): Promise<void> {
  const packageLockPath = join(waspAppDir, "package-lock.json");
  // A fresh app has no lockfile yet, so nothing in it is stale.
  if (!existsSync(packageLockPath)) {
    return;
  }
  const packageLock: { packages: Record<string, unknown> } = JSON.parse(
    await readFile(packageLockPath, "utf-8"),
  );
  const freshLockEntries = Object.entries(packageLock.packages).filter(
    ([packagePath]) => !isStaleWaspLockEntry(packagePath),
  );

  packageLock.packages = Object.fromEntries(freshLockEntries);
  await writeFile(packageLockPath, `${JSON.stringify(packageLock, null, 2)}\n`);
}

function isStaleWaspLockEntry(packagePath: string): boolean {
  return (
    packagePath.startsWith("node_modules/@wasp/cli-") ||
    WASP_PACKAGE_NAMES.some((packageName) =>
      isPathOrInside(packagePath, `node_modules/${packageName}`),
    )
  );
}

function isPathOrInside(packagePath: string, dirPath: string): boolean {
  return packagePath === dirPath || packagePath.startsWith(`${dirPath}/`);
}
