#!/usr/bin/env node
//MISE description="Run tests on Wasp packages (spec, deploy, sdk-core, etc.)."
/// <reference types="node" />

import {
  buildWaspPackages,
  getPackageDirPath,
  WASP_PACKAGE_NAMES,
} from "../../waspc/tools/packages/utils.ts";
import { getPackageJson, runCmd } from "../../waspc/tools/utils.ts";

testPackages();

function testPackages(): void {
  // Some packages' tests import the build output of the packages they depend on.
  console.log("Building packages");
  buildWaspPackages(WASP_PACKAGE_NAMES);
  for (const packageName of WASP_PACKAGE_NAMES) {
    testPackage(packageName);
  }
}

function testPackage(packageName: string): void {
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
