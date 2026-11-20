#!/usr/bin/env node
//MISE description="Run tests on Wasp packages (spec, deploy, etc.)."
/// <reference types="node" />

import {
  getPackageDirPath,
  WASP_PACKAGE_DIR_NAMES,
} from "../../waspc/tools/packages/utils.ts";
import { getPackageJson, runCmd } from "../../waspc/tools/utils.ts";

testPackages();

function testPackages(): void {
  for (const packageDirName of WASP_PACKAGE_DIR_NAMES) {
    testPackage(packageDirName);
  }
}

function testPackage(packageDirName: string): void {
  const packageDir = getPackageDirPath(packageDirName);
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
