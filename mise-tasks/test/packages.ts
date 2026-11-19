#!/usr/bin/env node
//MISE description="Run tests on Wasp packages (spec, deploy, etc.)."
/// <reference types="node" />

import {
  getPackageDirPath,
  isWaspLibPackageName,
  WASP_PACKAGE_NAMES,
} from "../../waspc/tools/packages/utils.ts";
import { getPackageJson, runCmd } from "../../waspc/tools/utils.ts";

testPackages();

function testPackages(): void {
  // `mise run test:libs` tests the Wasp libs.
  const packageNames = WASP_PACKAGE_NAMES.filter(
    (packageName) => !isWaspLibPackageName(packageName),
  );
  for (const packageName of packageNames) {
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
