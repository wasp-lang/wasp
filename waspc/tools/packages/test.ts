/// <reference types="node" />
// Helper to test the waspc/data/packages/* locally and in CI.

import { join } from "node:path";
import { discoverSubDirs, getPackageJson, getWaspcDirPath, runCmd } from "../utils.ts";

const waspcDirPath = getWaspcDirPath();
const dataPackagesDirPath = join(waspcDirPath, "data", "packages");

testPackages();

function testPackages(): void {
  const packageDirs = discoverSubDirs(dataPackagesDirPath);

  for (const packageDir of packageDirs) {
    testPackage(packageDir);
  }
}

function testPackage(packageDir: string): void {
  const packageJson = getPackageJson(packageDir);

  if (!packageJson.scripts?.test) {
    return;
  }

  console.log(`Testing ${packageJson.name} (${packageDir})`);

  runCmd("npm", ["install"], { cwd: packageDir, stdio: "inherit" });
  runCmd("npm", ["run", "test"], { cwd: packageDir, stdio: "inherit" });
}
