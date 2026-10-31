/// <reference types="node" />
// Helper to test the packages/* packages that waspc depends on, locally and in CI.

import { getPackageJson, runCmd } from "../utils.ts";
import {
  getPackageDirPath,
  WASP_NPM_PACKAGE_NAMES,
  WASPC_PACKAGES,
} from "./utils.ts";

testPackages();

function testPackages(): void {
  const packageNames = [
    ...WASPC_PACKAGES.map(({ name }) => name),
    ...WASP_NPM_PACKAGE_NAMES,
  ];
  for (const packageName of packageNames) {
    testPackage(packageName);
  }
}

function testPackage(name: string): void {
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
