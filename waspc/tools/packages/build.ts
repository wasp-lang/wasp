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
  getPackageDirPath,
  WASPC_PACKAGES,
  type WaspcPackage,
} from "./utils.ts";

// The files `waspc.cabal` lists as data files for each package. The lockfile
// exists only for the npm packages.
const PACKAGE_ARTIFACTS = ["dist", "package-lock.json"];

try {
  buildPackages();
} catch (e) {
  console.error(`ERROR: ${e instanceof Error ? e.message : String(e)}`);
  process.exitCode = 1;
}

function buildPackages(): void {
  for (const waspcPackage of WASPC_PACKAGES) {
    buildPackage(waspcPackage);
    copyPackageArtifactsToDataDir(waspcPackage.name);
  }
}

function buildPackage({ name, packageManager }: WaspcPackage): void {
  const packageDir = getPackageDirPath(name);
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
  const packageDir = getPackageDirPath(packageName);
  const dataPackageDir = join(getDataPackagesDirPath(), packageName);
  mkdirSync(dataPackageDir, { recursive: true });
  writePublishedPackageJson(packageDir, dataPackageDir);

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

// npm installs the data dir copy as a `file:` dependency: it does not
// understand `catalog:` and it installs devDependencies, so the copy gets the
// package.json `pnpm pack` writes, without them.
function writePublishedPackageJson(
  packageDir: string,
  dataPackageDir: string,
): void {
  const packageJson = readPackedPackageJson(packageDir);
  delete packageJson.devDependencies;
  writeFileSync(
    join(dataPackageDir, "package.json"),
    JSON.stringify(packageJson, undefined, 2) + "\n",
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

function haveSameContent(pathA: string, pathB: string): boolean {
  if (!existsSync(pathA) || !existsSync(pathB)) {
    return existsSync(pathA) === existsSync(pathB);
  }
  return readFileSync(pathA, "utf-8") === readFileSync(pathB, "utf-8");
}
