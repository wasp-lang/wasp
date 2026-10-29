/// <reference types="node" />
// Helper to test the packages/* packages that waspc depends on, locally and in CI.

import { getPackageJson, runCmd } from "../utils.ts";
import { getWaspcPackageDirPaths } from "./utils.ts";

testPackages();

function testPackages(): void {
  for (const packageDir of getWaspcPackageDirPaths()) {
    testPackage(packageDir);
  }
}

function testPackage(packageDir: string): void {
  const packageJson = getPackageJson(packageDir);

  if (!packageJson.scripts?.test) {
    return;
  }

  console.log(`Testing ${packageJson.name} (${packageDir})`);

  runCmd("npm", ["ci"], { cwd: packageDir, stdio: "inherit" });
  runCmd("npm", ["run", "test"], { cwd: packageDir, stdio: "inherit" });
}
