/// <reference types="node" />
// Helper to compile the packages/* packages that waspc depends on, locally and
// in CI, and copy their artifacts into the Cabal data dir.

import { cpSync, existsSync, mkdirSync, readFileSync, rmSync } from "node:fs";
import { join } from "node:path";
import {
  assertPackageVersionMatchesWaspc,
  getPackageJson,
  installWorkspaceDependencies,
  runCmd,
} from "../utils.ts";
import {
  getDataPackagesDirPath,
  getWaspcPackageDirPath,
  WASPC_PACKAGES,
  type WaspcPackage,
} from "./utils.ts";

// The files `waspc.cabal` lists as data files for each package. The lockfile
// exists only for the npm packages.
const PACKAGE_ARTIFACTS = ["dist", "package.json", "package-lock.json"];

try {
  buildPackages();
} catch (e) {
  console.error(`ERROR: ${e instanceof Error ? e.message : String(e)}`);
  process.exitCode = 1;
}

function buildPackages(): void {
  installWorkspaceDependencies();

  for (const waspcPackage of WASPC_PACKAGES) {
    buildPackage(waspcPackage);
    copyPackageArtifactsToDataDir(waspcPackage.name);
  }
}

function buildPackage({ name, packageManager }: WaspcPackage): void {
  const packageDir = getWaspcPackageDirPath(name);
  const { version } = getPackageJson(packageDir);

  assertPackageVersionMatchesWaspc(name, version);

  console.log(`Building ${name} package (${packageDir})`);

  if (packageManager === "npm") {
    runCmd("npm", ["install"], { cwd: packageDir });
  }
  runCmd(packageManager, ["run", "build"], { cwd: packageDir });
}

// Copies only the artifacts, so `node_modules` installed in the data dir copy
// (by `waspc` on first use) survives rebuilds. When the lockfile changed, that
// `node_modules` is stale, so we drop it and `waspc` installs it again.
function copyPackageArtifactsToDataDir(packageName: string): void {
  const packageDir = getWaspcPackageDirPath(packageName);
  const dataPackageDir = join(getDataPackagesDirPath(), packageName);
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
