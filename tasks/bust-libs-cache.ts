#!/usr/bin/env node
//MISE description="Busts the npm cache for all @wasp/lib-* packages in the current Wasp app. Must be run from a Wasp app directory."
//MISE dir="{{cwd}}"

import { execa } from "execa";
import { readFile, rm, writeFile } from "node:fs/promises";
import { join } from "node:path";
import { getWaspcDirPath } from "../waspc/tools/utils.ts";

const waspCliDevPath = join(getWaspcDirPath(), "tools", "wasp-cli-dev");
const $ = execa({ stdio: "inherit", verbose: "short" });

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
  await rm(generatedPath, { recursive: true, force: true });
}
await removeStaleGeneratedWaspLockEntries();
await $`${waspCliDevPath} install`;

// Compiling the project copies the new lib tarball to the out dir.
await $`${waspCliDevPath} compile`;

// The --force flag tells `npm` to ignore local cache when installing packages.
await $`npm install --force`;

type PackageLockEntry = { dependencies?: Record<string, string> };

// We remove .wasp/out before `wasp install`, so package-lock entries for
// generated workspaces and their local tarball deps become stale. Drop them so
// npm rebuilds those entries from the freshly generated package.json files.
// Apps generated before Wasp 0.27 used the `@wasp.sh` npm scope instead of
// `@wasp`, so we match both.
async function removeStaleGeneratedWaspLockEntries(): Promise<void> {
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
