/// <reference types="node" />
// Helper to compile the waspc/data/packages/*/ packages locally and in CI.

import { getWaspcVersion } from "../get-waspc-version.ts";
import { discoverSubDirs, getPackageJson, runCmd } from "../utils.ts";
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

  assertPackageVersionValid(packageName, packageVersion);

  console.log(`Building ${packageName} package (${packageDir})`);

  runCmd("npm", ["install"], { cwd: packageDir });
  runCmd("npm", ["run", "build"], { cwd: packageDir });
}

function assertPackageVersionValid(
  packageName: string,
  packageVersion: string,
): void {
  const waspcVersion = getWaspcVersion();

  if (packageVersion !== waspcVersion) {
    console.error(
      `ERROR: ${packageName} package version (${packageVersion}) != current Wasp version (${waspcVersion}).`,
    );
    console.error(
      `       Update the package version in package.json to ${waspcVersion}.`,
    );
    throw new Error();
  }
}
