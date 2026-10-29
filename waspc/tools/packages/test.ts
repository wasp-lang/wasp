/// <reference types="node" />
// Helper to test the packages/* packages locally and in CI.

import {
  getPackageJson,
  installWorkspaceDependencies,
  runCmd,
} from "../utils.ts";
import {
  getPackageDirPath,
  PUBLISHED_WASP_PACKAGE_NAMES,
  WASPC_PACKAGE_NAMES,
} from "./utils.ts";

testPackages();

function testPackages(): void {
  installWorkspaceDependencies();

  for (const packageName of [
    ...WASPC_PACKAGE_NAMES,
    ...PUBLISHED_WASP_PACKAGE_NAMES,
  ]) {
    testPackage(packageName);
  }
}

function testPackage(packageName: string) {
  const packageDir = getPackageDirPath(packageName);
  const packageJson = getPackageJson(packageDir);

  if (!packageJson.scripts?.test) {
    return;
  }

  console.log(`Testing ${packageJson.name} (${packageDir})`);

  runCmd("pnpm", ["run", "test"], {
    cwd: packageDir,
    stdio: "inherit",
  });
}
