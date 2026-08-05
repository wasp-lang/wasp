/// <reference types="node" />
// Helper to compile the waspc/packages/*/ packages locally and in CI.

import {
  assertPackageVersionMatchesWaspc,
  getPackageJson,
  getWaspcDirPath,
  runCmd,
} from "../utils.ts";
import { discoverPackageDirs } from "./utils.ts";

try {
  buildPackages();
} catch (e) {
  console.error(`ERROR: ${e instanceof Error ? e.message : String(e)}`);
  process.exitCode = 1;
}

function buildPackages(): void {
  // The packages are npm workspaces of `waspc/`, so a single install at the
  // workspace root installs the dependencies of all of them.
  runCmd("npm", ["install"], { cwd: getWaspcDirPath() });

  for (const packageDir of discoverPackageDirs()) {
    buildPackage(packageDir);
  }
}

function buildPackage(packageDir: string): void {
  const {
    name: packageName,
    version: packageVersion,
    private: isPrivate,
  } = getPackageJson(packageDir);

  // Private packages aren't published, so their version is meaningless.
  if (!isPrivate) {
    assertPackageVersionMatchesWaspc(packageName, packageVersion);
  }

  console.log(`Building ${packageName} package (${packageDir})`);

  runCmd("npm", ["run", "build"], { cwd: packageDir });
}
