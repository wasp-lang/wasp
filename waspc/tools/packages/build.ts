/// <reference types="node" />
// Helper to compile the packages/* packages that waspc depends on, locally and
// in CI, and copy their artifacts into the Cabal data dir.

import { cpSync, existsSync, mkdirSync, readFileSync, rmSync } from "node:fs";
import { basename, join } from "node:path";
import {
  assertPackageVersionMatchesWaspc,
  getPackageJson,
  runCmd,
} from "../utils.ts";
import { getDataPackagesDirPath, getWaspcPackageDirPaths } from "./utils.ts";

// The files `waspc.cabal` lists as data files for each package.
const PACKAGE_ARTIFACTS = ["dist", "package.json", "package-lock.json"];

try {
  buildPackages();
} catch (e) {
  console.error(`ERROR: ${e instanceof Error ? e.message : String(e)}`);
  process.exitCode = 1;
}

function buildPackages(): void {
  for (const packageDir of getWaspcPackageDirPaths()) {
    buildPackage(packageDir);
    copyPackageArtifactsToDataDir(packageDir);
  }
}

function buildPackage(packageDir: string): void {
  const { name: packageName, version: packageVersion } =
    getPackageJson(packageDir);

  assertPackageVersionMatchesWaspc(packageName, packageVersion);

  console.log(`Building ${packageName} package (${packageDir})`);

  runCmd("npm", ["install"], { cwd: packageDir });
  runCmd("npm", ["run", "build"], { cwd: packageDir });
}

// Copies only the artifacts, so `node_modules` installed in the data dir copy
// (by `waspc` on first use) survives rebuilds. When the lockfile changed, that
// `node_modules` is stale, so we drop it and `waspc` installs it again.
function copyPackageArtifactsToDataDir(packageDir: string): void {
  const dataPackageDir = join(getDataPackagesDirPath(), basename(packageDir));
  mkdirSync(dataPackageDir, { recursive: true });

  const lockFileName = "package-lock.json";
  const lockFileChanged = !haveSameContent(
    join(packageDir, lockFileName),
    join(dataPackageDir, lockFileName),
  );

  for (const artifact of PACKAGE_ARTIFACTS) {
    const src = join(packageDir, artifact);
    const dst = join(dataPackageDir, artifact);
    rmSync(dst, { recursive: true, force: true });
    if (existsSync(src)) {
      cpSync(src, dst, { recursive: true });
    }
  }

  if (lockFileChanged) {
    rmSync(join(dataPackageDir, "node_modules"), {
      recursive: true,
      force: true,
    });
  }
}

function haveSameContent(pathA: string, pathB: string): boolean {
  if (!existsSync(pathA) || !existsSync(pathB)) {
    return existsSync(pathA) === existsSync(pathB);
  }
  return readFileSync(pathA, "utf-8") === readFileSync(pathB, "utf-8");
}
