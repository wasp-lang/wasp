/// <reference types="node" />
// Helper to test the packages/* packages locally and in CI.

import { getPackageJson, runCmd } from "../utils.ts";
import { getPackageDirPath, WASP_PACKAGE_NAMES } from "./utils.ts";

testPackages();

function testPackages(): void {
  for (const packageName of WASP_PACKAGE_NAMES) {
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
