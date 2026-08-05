/// <reference types="node" />
// Helper to test the waspc/packages/* locally and in CI.

import { getPackageJson, getWaspcDirPath, runCmd } from "../utils.ts";
import { discoverPackageDirs } from "./utils.ts";

testPackages();

function testPackages(): void {
  // The packages are npm workspaces of `waspc/`, so a single install at the
  // workspace root installs the dependencies of all of them.
  runCmd("npm", ["install"], { cwd: getWaspcDirPath(), stdio: "inherit" });

  for (const packageDir of discoverPackageDirs()) {
    testPackage(packageDir);
  }
}

function testPackage(packageDir: string): void {
  const packageJson = getPackageJson(packageDir);

  if (!packageJson.scripts?.test) {
    return;
  }

  console.log(`Testing ${packageJson.name} (${packageDir})`);

  runCmd("npm", ["run", "test"], { cwd: packageDir, stdio: "inherit" });
}
