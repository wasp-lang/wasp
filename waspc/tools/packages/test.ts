/// <reference types="node" />
// Helper to test the packages/* packages that waspc depends on, locally and in CI.

import {
  getPackageJson,
  installWorkspaceDependencies,
  runCmd,
} from "../utils.ts";
import {
  getWaspcPackageDirPath,
  WASPC_NPM_PACKAGE_NAMES,
  WASPC_WORKSPACE_PACKAGE_NAMES,
} from "./utils.ts";

testPackages();

function testPackages(): void {
  installWorkspaceDependencies();

  for (const packageName of WASPC_WORKSPACE_PACKAGE_NAMES) {
    testPackage(packageName, "pnpm");
  }

  for (const packageName of WASPC_NPM_PACKAGE_NAMES) {
    runCmd("npm", ["ci"], {
      cwd: getWaspcPackageDirPath(packageName),
      stdio: "inherit",
    });
    testPackage(packageName, "npm");
  }
}

function testPackage(packageName: string, packageManager: "pnpm" | "npm") {
  const packageDir = getWaspcPackageDirPath(packageName);
  const packageJson = getPackageJson(packageDir);

  if (!packageJson.scripts?.test) {
    return;
  }

  console.log(`Testing ${packageJson.name} (${packageDir})`);

  runCmd(packageManager, ["run", "test"], {
    cwd: packageDir,
    stdio: "inherit",
  });
}
