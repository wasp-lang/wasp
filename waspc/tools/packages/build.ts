/// <reference types="node" />
// Helper to compile the packages/* packages that waspc depends on, locally and
// in CI, and copy their artifacts into the Cabal data dir.

import {
  cpSync,
  existsSync,
  mkdirSync,
  mkdtempSync,
  readdirSync,
  readFileSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { extract } from "tar";
import {
  assertPackageVersionMatchesWaspc,
  getPackageJson,
  runCmd,
  type PackageJson,
} from "../utils.ts";
import {
  getDataPackagesDirPath,
  getWaspcPackageDirPath,
  WASPC_PACKAGES,
  type WaspcPackage,
} from "./utils.ts";

// The files `waspc.cabal` lists as data files for each package, next to the
// package.json and the lockfile written below.
const PACKAGE_ARTIFACTS = ["dist"];

try {
  buildPackages();
} catch (e) {
  console.error(`ERROR: ${e instanceof Error ? e.message : String(e)}`);
  process.exitCode = 1;
}

function buildPackages(): void {
  for (const waspcPackage of WASPC_PACKAGES) {
    buildPackage(waspcPackage.name);
    copyPackageArtifactsToDataDir(waspcPackage);
  }
}

function buildPackage(packageName: string): void {
  const packageDir = getWaspcPackageDirPath(packageName);
  const { version } = getPackageJson(packageDir);

  assertPackageVersionMatchesWaspc(packageName, version);

  console.log(`Building ${packageName} package (${packageDir})`);

  runCmd("pnpm", ["run", "build"], { cwd: packageDir });
}

// Copies only the artifacts, so `node_modules` installed in the data dir copy
// (by `waspc` on first use) survives rebuilds. When the package.json changed,
// the lockfile and that `node_modules` are stale, so we write a new lockfile
// and drop `node_modules`, and `waspc` installs it again.
function copyPackageArtifactsToDataDir({
  name,
  installedByWaspc,
}: WaspcPackage): void {
  const packageDir = getWaspcPackageDirPath(name);
  const dataPackageDir = join(getDataPackagesDirPath(), name);
  mkdirSync(dataPackageDir, { recursive: true });

  const packageJsonChanged = writePublishedPackageJson(
    packageDir,
    dataPackageDir,
  );

  for (const artifact of PACKAGE_ARTIFACTS) {
    const src = join(packageDir, artifact);
    const dst = join(dataPackageDir, artifact);
    rmSync(dst, { recursive: true, force: true });
    if (existsSync(src)) {
      cpSync(src, dst, { recursive: true });
    }
  }

  const lockFilePath = join(dataPackageDir, "package-lock.json");
  if (installedByWaspc && (packageJsonChanged || !existsSync(lockFilePath))) {
    writeNpmLockFile(dataPackageDir);
    rmSync(join(dataPackageDir, "node_modules"), {
      recursive: true,
      force: true,
    });
  }
}

// The data dir copy is installed with npm, which needs a lockfile of its own
// since the workspace's pnpm lockfile is not shipped.
function writeNpmLockFile(dataPackageDir: string): void {
  console.log(`Writing npm lockfile for ${dataPackageDir}`);
  runCmd("npm", ["install", "--package-lock-only", "--ignore-scripts"], {
    cwd: dataPackageDir,
  });
}

// npm installs the data dir copy: it does not understand `catalog:` and it
// would install devDependencies, so the copy gets the package.json `pnpm pack`
// writes, without them. Returns whether the written package.json differs from
// the one that was there.
function writePublishedPackageJson(
  packageDir: string,
  dataPackageDir: string,
): boolean {
  const packageJson = readPackedPackageJson(packageDir);
  delete packageJson.devDependencies;
  const packageJsonPath = join(dataPackageDir, "package.json");
  const content = JSON.stringify(packageJson, undefined, 2) + "\n";
  const changed =
    !existsSync(packageJsonPath) ||
    readFileSync(packageJsonPath, "utf-8") !== content;
  writeFileSync(packageJsonPath, content);
  return changed;
}

function readPackedPackageJson(packageDir: string): PackageJson {
  const packDir = mkdtempSync(join(tmpdir(), "wasp-package-"));
  try {
    runCmd("pnpm", ["pack", "--pack-destination", packDir], {
      cwd: packageDir,
    });
    const tarball = readdirSync(packDir).find((file) => file.endsWith(".tgz"));
    if (tarball === undefined) {
      throw new Error(`pnpm pack produced no tarball in ${packDir}`);
    }
    extract({ file: join(packDir, tarball), cwd: packDir, sync: true }, [
      "package/package.json",
    ]);
    return JSON.parse(
      readFileSync(join(packDir, "package", "package.json"), "utf-8"),
    );
  } finally {
    rmSync(packDir, { recursive: true, force: true });
  }
}
