#!/usr/bin/env node
//MISE description="Busts the npm cache for all @wasp/lib-* packages in the current Wasp app. Must be run from a Wasp app directory."
//MISE dir="{{cwd}}"

import { execa } from "execa";
import { readFileSync, rmSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import { getWaspcDirPath } from "../utils.ts";

const waspCliDevPath = join(getWaspcDirPath(), "tools", "wasp-cli-dev");
const execaOptions = { stdio: "inherit", verbose: "short" } as const;

const GENERATED_WASP_PATHS = [
  ".wasp/out",
  "node_modules/@wasp/generated-server",
  "node_modules/@wasp.sh/generated-server",
  "node_modules/wasp",
];

// `wasp install` must run before compile so @wasp/spec matches CLI version.
// Old generated workspaces point at old lib tarballs, so remove them from disk
// and lockfile first. Compile then regenerates .wasp/out with fresh tarballs.
for (const generatedPath of GENERATED_WASP_PATHS) {
  rmSync(generatedPath, { recursive: true, force: true });
}
removeStaleGeneratedWaspLockEntries();
await execa(waspCliDevPath, ["install"], execaOptions);

// Compiling the project copies the new lib tarball to the out dir.
await execa(waspCliDevPath, ["compile"], execaOptions);

// The --force flag tells `npm` to ignore local cache when installing packages.
await execa("npm", ["install", "--force"], execaOptions);

type PackageLockEntry = { dependencies?: Record<string, string> };

// We remove .wasp/out before `wasp install`, so package-lock entries for
// generated workspaces and their local tarball deps become stale. Drop them so
// npm rebuilds those entries from the freshly generated package.json files.
// Apps generated before Wasp 0.27 used the `@wasp.sh` npm scope instead of
// `@wasp`, so we match both.
function removeStaleGeneratedWaspLockEntries(): void {
  const packageLockPath = "package-lock.json";
  const packageLock: { packages: Record<string, PackageLockEntry> } =
    JSON.parse(readFileSync(packageLockPath, "utf-8"));
  const freshLockEntries = Object.entries(packageLock.packages)
    .filter(([packagePath]) => !isStaleGeneratedWaspPackage(packagePath))
    .map(([packagePath, lockEntry]) => [
      packagePath,
      withoutWaspLibDependencies(lockEntry),
    ]);

  packageLock.packages = Object.fromEntries(freshLockEntries);
  writeFileSync(packageLockPath, `${JSON.stringify(packageLock, null, 2)}\n`);
}

function isStaleGeneratedWaspPackage(packagePath: string): boolean {
  const isWaspLib =
    packagePath.startsWith("node_modules/") &&
    isWaspLibName(packagePath.slice("node_modules/".length));
  const isGenerated = GENERATED_WASP_PATHS.some(
    (generatedPath) =>
      packagePath === generatedPath ||
      packagePath.startsWith(`${generatedPath}/`),
  );
  return isWaspLib || isGenerated;
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
