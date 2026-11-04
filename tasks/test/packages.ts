#!/usr/bin/env node
//MISE description="Run tests on Wasp packages (spec, deploy, etc.)."
/// <reference types="node" />

import {
  getPackageDirPath,
  WASPC_PACKAGES,
  type WaspcPackage,
} from "../../waspc/tools/packages/utils.ts";
import { getPackageJson, runCmd } from "../../waspc/tools/utils.ts";

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
