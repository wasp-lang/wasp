import { readFile, writeFile } from "node:fs/promises";

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
const DEV_WASP_NPM_PACKAGE_NAMES = [
  "@wasp/wasp",
  "@wasp/cli",
  "@wasp/spec",
  "@wasp/deploy",
  "@wasp/studio",
];

type PackageLockEntry = { dependencies?: Record<string, string> };

/**
 * Drops the stale entries from the `package-lock.json` in the current dir, so
 * the next `npm install` rebuilds them from the package.json files.
 *
 * The entries of generated workspaces and their tarball deps go stale once
 * `.wasp/out` is gone. The entries of the dev Wasp npm packages go stale
 * whenever `mise run build:dev-packages` repacks their tarballs. Apps generated
 * before Wasp 0.27 used the `@wasp.sh` npm scope instead of `@wasp`, so we
 * match both.
 */
export async function removeStaleGeneratedWaspLockEntries(): Promise<void> {
  const packageLockPath = "package-lock.json";
  const packageLock: { packages: Record<string, PackageLockEntry> } =
    JSON.parse(await readFile(packageLockPath, "utf-8"));
  const freshLockEntries = Object.entries(packageLock.packages)
    .filter(([packagePath]) => !isStaleGeneratedWaspPackage(packagePath))
    .map(([packagePath, lockEntry]) => [
      packagePath,
      withoutWaspLibDependencies(lockEntry),
    ]);

  packageLock.packages = Object.fromEntries(freshLockEntries);
  await writeFile(packageLockPath, `${JSON.stringify(packageLock, null, 2)}\n`);
}

function isStaleGeneratedWaspPackage(packagePath: string): boolean {
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
