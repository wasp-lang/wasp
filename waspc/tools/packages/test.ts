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

function testPackage({ name }: WaspcPackage): void {
  const packageDir = getPackageDirPath(name);
  const packageJson = getPackageJson(packageDir);

  if (!packageJson.scripts?.test) {
    return;
  }

  console.log(`Testing ${name} (${packageDir})`);

  runCmd("pnpm", ["run", "test"], {
    cwd: packageDir,
    stdio: "inherit",
  });
}
