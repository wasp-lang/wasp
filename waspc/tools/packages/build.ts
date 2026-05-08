/// <reference types="node" />
// Helper to compile the waspc/data/packages/*/ packages locally and in CI.

import {
  assertPackageVersionMatchesWaspc,
  discoverSubDirs,
  getPackageJson,
  runCmd,
} from "../utils.ts";
import { getDataPackagesDirPath } from "./utils.ts";

buildPackages();

function buildPackages(): void {
  const dataPackagesDirPath = getDataPackagesDirPath();
  const packageDirs = discoverSubDirs(dataPackagesDirPath);

  for (const packageDir of packageDirs) {
    buildPackage(packageDir);
  }
}

function buildPackage(packageDir: string): void {
  const { name: packageName, version: packageVersion } =
    getPackageJson(packageDir);

  assertPackageVersionMatchesWaspc(packageName, packageVersion);

  console.log(`Building ${packageName} package (${packageDir})`);

  runCmd("npm", ["install"], { cwd: packageDir });
  runCmd("npm", ["run", "build"], { cwd: packageDir });
}
