import { existsSync } from "node:fs";
import { readFile, writeFile } from "node:fs/promises";
import { join } from "node:path";
import { getPackageName, WASP_PACKAGE_DIR_NAMES } from "./packages/utils.ts";

/** What Wasp generates into a Wasp app, relative to the app's dir. */
export const GENERATED_WASP_PATHS = [
  ".wasp/out",
  "node_modules/@wasp/generated-server",
  "node_modules/@wasp.sh/generated-server",
  "node_modules/wasp",
];

// The dev Wasp npm packages, which the repo's Wasp apps install from `file:`
// tarballs (see `tasks/build/dev-packages.ts`).
// TODO: Drop these once the repo's Wasp apps join the pnpm workspace.
const DEV_WASP_NPM_PACKAGE_NAMES = WASP_PACKAGE_DIR_NAMES.map(getPackageName);

type PackageLockEntry = { dependencies?: Record<string, string> };

/**
 * Generated workspaces go stale once `.wasp/out` is gone, and the dev Wasp npm
 * packages whenever they're repacked. Apps from before Wasp 0.27 use the
 * `@wasp.sh` scope, so we match it too.
 */
export async function removeStaleWaspLockEntries(
  waspAppDir: string,
): Promise<void> {
  const packageLockPath = join(waspAppDir, "package-lock.json");
  // A fresh app has no lockfile yet, so nothing in it is stale.
  if (!existsSync(packageLockPath)) {
    return;
  }
  const packageLock: { packages: Record<string, PackageLockEntry> } =
    JSON.parse(await readFile(packageLockPath, "utf-8"));
  const freshLockEntries = Object.entries(packageLock.packages)
    .filter(([packagePath]) => !isStaleWaspLockEntry(packagePath))
    .map(([packagePath, lockEntry]) => [
      packagePath,
      withoutWaspLibDependencies(lockEntry),
    ]);

  packageLock.packages = Object.fromEntries(freshLockEntries);
  await writeFile(packageLockPath, `${JSON.stringify(packageLock, null, 2)}\n`);
}

function isStaleWaspLockEntry(packagePath: string): boolean {
  const isWaspLib =
    packagePath.startsWith("node_modules/") &&
    isWaspLibName(packagePath.slice("node_modules/".length));
  const isGenerated = GENERATED_WASP_PATHS.some((generatedPath) =>
    isPathOrInside(packagePath, generatedPath),
  );
  const isDevWaspNpmPackage =
    packagePath.startsWith("node_modules/@wasp/cli-") ||
    DEV_WASP_NPM_PACKAGE_NAMES.some((packageName) =>
      isPathOrInside(packagePath, `node_modules/${packageName}`),
    );
  return isWaspLib || isGenerated || isDevWaspNpmPackage;
}

function isPathOrInside(packagePath: string, dirPath: string): boolean {
  return packagePath === dirPath || packagePath.startsWith(`${dirPath}/`);
}

function withoutWaspLibDependencies(
  lockEntry: PackageLockEntry,
): PackageLockEntry {
  if (!lockEntry.dependencies) {
    return lockEntry;
  }
  const dependencies = Object.fromEntries(
    Object.entries(lockEntry.dependencies).filter(
      ([dependencyName]) => !isWaspLibName(dependencyName),
    ),
  );
  return { ...lockEntry, dependencies };
}

function isWaspLibName(packageName: string): boolean {
  return /^@wasp(\.sh)?\/lib-/.test(packageName);
}
