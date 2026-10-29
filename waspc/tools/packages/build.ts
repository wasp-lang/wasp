/// <reference types="node" />
// Helper to compile the packages/* packages that waspc ships in its data dir,
// locally and in CI, and copy their artifacts there.

import { cpSync, existsSync, mkdirSync, rmSync } from "node:fs";
import { join } from "node:path";
import {
  assertPackageVersionMatchesWaspc,
  getPackageJson,
  installWorkspaceDependencies,
  runCmd,
} from "../utils.ts";
import {
  getDataPackagesDirPath,
  getPackageDirPath,
  WASPC_WORKSPACE_PACKAGE_NAMES,
} from "./utils.ts";

// The files `waspc.cabal` lists as data files for each package.
const PACKAGE_ARTIFACTS = ["dist", "package.json"];

try {
  buildPackages();
} catch (e) {
  console.error(`ERROR: ${e instanceof Error ? e.message : String(e)}`);
  process.exitCode = 1;
}

function buildPackages(): void {
  installWorkspaceDependencies();

  for (const packageName of WASPC_WORKSPACE_PACKAGE_NAMES) {
    buildPackage(packageName);
    copyPackageArtifactsToDataDir(packageName);
  }
}

function buildPackage(packageName: string) {
  const packageDir = getPackageDirPath(packageName);
  const { name, version } = getPackageJson(packageDir);

  assertPackageVersionMatchesWaspc(name, version);

  console.log(`Building ${name} package (${packageDir})`);

  runCmd("pnpm", ["run", "build"], { cwd: packageDir });
}

function copyPackageArtifactsToDataDir(packageName: string): void {
  const packageDir = getPackageDirPath(packageName);
  const dataPackageDir = join(getDataPackagesDirPath(), packageName);
  mkdirSync(dataPackageDir, { recursive: true });

  for (const artifact of PACKAGE_ARTIFACTS) {
    const src = join(packageDir, artifact);
    const dst = join(dataPackageDir, artifact);
    rmSync(dst, { recursive: true, force: true });
    if (existsSync(src)) {
      cpSync(src, dst, { recursive: true });
    }
  }
}
