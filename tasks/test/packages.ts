#!/usr/bin/env node
//MISE description="Run tests on Wasp packages (spec, deploy, sdk-core, etc.)."
/// <reference types="node" />

import {
  getPackageDirPath,
  WASP_PACKAGE_NAMES,
} from "../../waspc/tools/packages/utils.ts";
import {
  getPackageJson,
  getRepoRootPath,
  runCmd,
} from "../../waspc/tools/utils.ts";

testPackages();

function testPackages(): void {
  buildPackages();
  for (const packageName of WASP_PACKAGE_NAMES) {
    testPackage(packageName);
  }
}

// Some packages' tests import the build output of the packages they depend on.
// pnpm builds them in dependency order.
function buildPackages(): void {
  console.log("Building packages");
  runCmd(
    "pnpm",
    [
      ...WASP_PACKAGE_NAMES.flatMap((packageName) => ["--filter", packageName]),
      "--fail-if-no-match",
      "run",
      "--if-present",
      "build",
    ],
    { cwd: getRepoRootPath(), stdio: "inherit" },
  );
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
