/// <reference types="node" />
// Helper to test the packages/* packages that waspc depends on, locally and in CI.

import { getPackageJson, runCmd } from "../utils.ts";
import {
  getPackageDirPath,
  WASPC_PACKAGES,
  type WaspcPackage,
} from "./utils.ts";

testPackages();

function testPackages(): void {
  for (const waspcPackage of WASPC_PACKAGES) {
    testPackage(waspcPackage);
  }
}

function testPackage({ name, packageManager }: WaspcPackage): void {
  const packageDir = getPackageDirPath(name);
  const packageJson = getPackageJson(packageDir);

  if (!packageJson.scripts?.test) {
    return;
  }

  console.log(`Testing ${name} (${packageDir})`);

  if (packageManager === "npm") {
    runCmd("npm", ["ci"], { cwd: packageDir, stdio: "inherit" });
  }
  runCmd(packageManager, ["run", "test"], {
    cwd: packageDir,
    stdio: "inherit",
  });
}
