/// <reference types="node" />
// Helper to test the waspc/data/packages/* locally and in CI.

import { discoverSubDirs, getPackageJson, runCmd } from "../utils.ts";
import { getDataPackagesDirPath, orderPackageDirs } from "./utils.ts";

testPackages();

function testPackages(): void {
  // Package test scripts build their package, so dependency order matters:
  // module-builder resolves @wasp.sh/spec/compiler from spec's built dist.
  const packageDirs = orderPackageDirs(
    discoverSubDirs(getDataPackagesDirPath()),
  );

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
