/// <reference types="node" />
// Helper to compile the waspc/data/packages/*/ packages locally and in CI.

import {
  assertPackageVersionMatchesWaspc,
  discoverSubDirs,
  getPackageJson,
  runCmd,
} from "../utils.ts";
import { getDataPackagesDirPath } from "./utils.ts";

try {
  buildPackages();
} catch (e) {
  console.error(`ERROR: ${e instanceof Error ? e.message : String(e)}`);
  process.exitCode = 1;
}

function buildPackages(): void {
  const dataPackagesDirPath = getDataPackagesDirPath();
  const packageDirs = orderPackageDirs(discoverSubDirs(dataPackagesDirPath));

  for (const packageDir of packageDirs) {
    buildPackage(packageDir);
  }
}

function orderPackageDirs(packageDirs: string[]): string[] {
  const packageDirsWithNames = packageDirs.map((dir) => ({
    dir,
    name: getPackageJson(dir).name,
  }));

  packageDirsWithNames.sort((a, b) => {
    // module-builder imports @wasp.sh/spec/internal from spec's built dist.
    if (a.name === "@wasp.sh/spec") return -1;
    if (b.name === "@wasp.sh/spec") return 1;
    return a.dir.localeCompare(b.dir);
  });

  return packageDirsWithNames.map(({ dir }) => dir);
}

function buildPackage(packageDir: string): void {
  const { name: packageName, version: packageVersion } =
    getPackageJson(packageDir);

  assertPackageVersionMatchesWaspc(packageName, packageVersion);

  console.log(`Building ${packageName} package (${packageDir})`);

  runCmd("npm", ["install"], { cwd: packageDir });
  runCmd("npm", ["run", "build"], { cwd: packageDir });
}
