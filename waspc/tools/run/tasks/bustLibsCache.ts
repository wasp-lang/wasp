/// <reference types="node" />

import { readFileSync, rmSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import { isWaspProjectDir } from "../../utils.ts";
import { runCmd } from "../runCmd.ts";
import { runWaspCli } from "./waspCli.ts";

export async function bustLibsCache(appDir: string): Promise<void> {
  if (!isWaspProjectDir(appDir)) {
    throw new Error(`${appDir} is not a Wasp app directory.`);
  }

  // `wasp install` must run before compile so @wasp/spec matches CLI version.
  // Old generated workspaces point at old lib tarballs, so remove them from disk
  // and lockfile first. Compile then regenerates .wasp/out with fresh tarballs.
  removeGeneratedPaths(appDir);
  removeStaleGeneratedLockEntries(appDir);
  await runWaspCli(["install"], appDir);

  // Compiling the project copies the new lib tarball to the out dir.
  await runWaspCli(["compile"], appDir);

  // The --force flag tells `npm` to ignore local cache when installing packages.
  await runCmd("npm", ["install", "--force"], { cwd: appDir });
}

const REMOVED_GENERATED_PATHS = [
  ".wasp/out",
  "node_modules/@wasp/generated-server",
  "node_modules/@wasp.sh/generated-server",
  "node_modules/wasp",
];

function removeGeneratedPaths(appDir: string): void {
  for (const removedPath of REMOVED_GENERATED_PATHS) {
    rmSync(join(appDir, removedPath), { recursive: true, force: true });
  }
}

type PackageLockEntry = { dependencies?: Record<string, string> };
type PackageLock = { packages: Record<string, PackageLockEntry> };

// We remove .wasp/out before `wasp install`, so package-lock entries for
// generated workspaces and their local tarball deps become stale. Drop them so
// npm rebuilds those entries from the freshly generated package.json files.
// Apps generated before Wasp 0.27 used the `@wasp.sh` npm scope instead of
// `@wasp`, so we match both.
function removeStaleGeneratedLockEntries(appDir: string): void {
  const packageLockPath = join(appDir, "package-lock.json");
  const packageLock: PackageLock = JSON.parse(
    readFileSync(packageLockPath, "utf-8"),
  );

  packageLock.packages = Object.fromEntries(
    Object.entries(packageLock.packages)
      .filter(([packagePath]) => !isStaleGeneratedPackage(packagePath))
      .map(([packagePath, lockEntry]) => [
        packagePath,
        withoutWaspLibDependencies(lockEntry),
      ]),
  );

  writeFileSync(packageLockPath, `${JSON.stringify(packageLock, null, 2)}\n`);
}

function isStaleGeneratedPackage(packagePath: string): boolean {
  return (
    isWaspLibInNodeModules(packagePath) ||
    isUnderRemovedGeneratedPath(packagePath)
  );
}

function isWaspLibInNodeModules(packagePath: string): boolean {
  const nodeModulesPrefix = "node_modules/";
  return (
    packagePath.startsWith(nodeModulesPrefix) &&
    isWaspLibName(packagePath.slice(nodeModulesPrefix.length))
  );
}

function isUnderRemovedGeneratedPath(packagePath: string): boolean {
  return REMOVED_GENERATED_PATHS.some(
    (removedPath) =>
      packagePath === removedPath || packagePath.startsWith(`${removedPath}/`),
  );
}

function withoutWaspLibDependencies(
  lockEntry: PackageLockEntry,
): PackageLockEntry {
  if (!lockEntry.dependencies) {
    return lockEntry;
  }

  const nonWaspLibDependencies = Object.fromEntries(
    Object.entries(lockEntry.dependencies).filter(
      ([dependencyName]) => !isWaspLibName(dependencyName),
    ),
  );
  return { ...lockEntry, dependencies: nonWaspLibDependencies };
}

function isWaspLibName(packageName: string): boolean {
  return (
    packageName.startsWith("@wasp/lib-") ||
    packageName.startsWith("@wasp.sh/lib-")
  );
}
