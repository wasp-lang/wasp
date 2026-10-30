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
} from "./utils.ts";

try {
  buildPackages();
} catch (e) {
  console.error(`ERROR: ${e instanceof Error ? e.message : String(e)}`);
  process.exitCode = 1;
}

function buildPackages(): void {
  for (const { name, kind } of WASPC_PACKAGES) {
    const packageDir = getWaspcPackageDirPath(name);
    const dataPackageDir = join(getDataPackagesDirPath(), name);

    buildPackage(name, packageDir);
    mkdirSync(dataPackageDir, { recursive: true });
    const packageJsonChanged = writePublishedPackageJson(
      packageDir,
      dataPackageDir,
    );
    if (kind === "runnable" && packageJsonChanged) {
      removeNpmLockFile(dataPackageDir);
    }
    copyDistToDataDir(packageDir, dataPackageDir);
    if (
      kind === "runnable" &&
      !existsSync(getNpmLockFilePath(dataPackageDir))
    ) {
      writeNpmLockFile(dataPackageDir);
    }
  }
}

function getNpmLockFilePath(dataPackageDir: string): string {
  return join(dataPackageDir, "package-lock.json");
}

function buildPackage(packageName: string, packageDir: string): void {
  const { version } = getPackageJson(packageDir);
  assertPackageVersionMatchesWaspc(packageName, version);

  console.log(`Building ${packageName} package (${packageDir})`);
  runCmd("pnpm", ["run", "build"], { cwd: packageDir });
}

// npm installs the data dir copy: it does not understand `catalog:` and it
// would install devDependencies, so the copy gets the package.json `pnpm pack`
// writes, without them. Returns whether it differs from the one already there,
// which is what decides whether the lockfile and `node_modules` next to it are
// still valid.
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

// Only `dist` is replaced, so `node_modules` installed in the data dir copy
// (by `waspc` on first use) survives rebuilds.
function copyDistToDataDir(packageDir: string, dataPackageDir: string): void {
  const src = join(packageDir, "dist");
  const dst = join(dataPackageDir, "dist");
  rmSync(dst, { recursive: true, force: true });
  cpSync(src, dst, { recursive: true });
}

// A changed package.json makes the lockfile stale. Removing it right after the
// package.json is written means an interrupted or failed build leaves the
// lockfile missing, which the next build sees and regenerates, instead of a
// new package.json next to an old lockfile it would keep.
function removeNpmLockFile(dataPackageDir: string): void {
  rmSync(getNpmLockFilePath(dataPackageDir), { force: true });
}

// `waspc` installs the data dir copy with npm, which needs a lockfile of its
// own since the workspace's pnpm lockfile is not shipped. A `node_modules`
// already there was installed from a previous lockfile, so it goes, and
// `waspc` installs it again on first use.
// TODO: The versions get resolved on every build instead of being committed
// and reviewed. Goes away with #4434, when these packages become dependencies
// of the Wasp npm package and the project's lockfile pins them.
// npm reads its project config from the nearest package.json dir, so the
// repo's `.npmrc` does not apply here and the release window is passed by hand.
function writeNpmLockFile(dataPackageDir: string): void {
  console.log(`Writing npm lockfile for ${dataPackageDir}`);
  rmSync(join(dataPackageDir, "node_modules"), {
    recursive: true,
    force: true,
  });
  runCmd(
    "npm",
    [
      "install",
      "--package-lock-only",
      "--ignore-scripts",
      "--min-release-age=7",
    ],
    { cwd: dataPackageDir },
  );
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
