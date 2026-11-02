/// <reference types="node" />

import { readFileSync, rmSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import type { Command } from "../command.ts";
import { runCmd } from "../runCmd.ts";
import { runWaspCli } from "../waspCliDev.ts";

export const waspAppCommands: Command[] = [
  {
    name: "bust-libs-cache",
    description:
      "Busts the npm cache for all @wasp/lib-* packages in the current Wasp app. Must be run from a Wasp app directory.",
    run: () => bustLibsCache(process.cwd()),
  },
  {
    name: "wasp-cli",
    argsHint: "<args>",
    description:
      "Runs the dev version of wasp executable in the current directory while forwarding arguments. Builds the project (hs) first if needed.",
    run: (args) => runWaspCli(args, process.cwd()),
  },
];

const GENERATED_WASP_PATHS = [
  ".wasp/out",
  "node_modules/@wasp/generated-server",
  "node_modules/@wasp.sh/generated-server",
  "node_modules/wasp",
];

async function bustLibsCache(appDir: string): Promise<void> {
  // `wasp install` must run before compile so @wasp/spec matches CLI version.
  // Old generated workspaces point at old lib tarballs, so remove them from disk
  // and lockfile first. Compile then regenerates .wasp/out with fresh tarballs.
  for (const generatedPath of GENERATED_WASP_PATHS) {
    rmSync(join(appDir, generatedPath), { recursive: true, force: true });
  }
  removeStaleGeneratedWaspLockEntries(appDir);
  await runWaspCli(["install"], appDir);

  // Compiling the project copies the new lib tarball to the out dir.
  await runWaspCli(["compile"], appDir);

  // The --force flag tells `npm` to ignore local cache when installing packages.
  await runCmd("npm", ["install", "--force"], { cwd: appDir });
}

type PackageLockEntry = { dependencies?: Record<string, string> };

// We remove .wasp/out before `wasp install`, so package-lock entries for
// generated workspaces and their local tarball deps become stale. Drop them so
// npm rebuilds those entries from the freshly generated package.json files.
// Apps generated before Wasp 0.27 used the `@wasp.sh` npm scope instead of
// `@wasp`, so we match both.
function removeStaleGeneratedWaspLockEntries(appDir: string): void {
  const packageLockPath = join(appDir, "package-lock.json");
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
