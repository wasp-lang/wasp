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
import { gunzipSync } from "node:zlib";
import {
  assertPackageVersionMatchesWaspc,
  getPackageJson,
  installWorkspaceDependencies,
  runCmd,
  type PackageJson,
} from "../utils.ts";
import {
  getDataPackagesDirPath,
  getWaspcPackageDirPath,
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

// npm installs the data dir copy into projects as a `file:` dependency, which
// it treats like a workspace: it resolves the copy's devDependencies too, and
// it does not understand pnpm's `catalog:` and `workspace:` specifiers. So the
// copy gets the package.json `pnpm pack` writes (real versions in) without its
// devDependencies, which a project has no use for.
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
    return JSON.parse(
      readFileFromTarball(join(packDir, tarball), "package/package.json"),
    );
  } finally {
    rmSync(packDir, { recursive: true, force: true });
  }
}

// This script runs before the workspace is installed, so it cannot use a tar
// package, and the `tar` binary differs across our CI runners. A tar archive
// is a sequence of 512-byte headers (the entry's path in the first 100 bytes,
// its size as octal text at bytes 124 to 136), each followed by the entry's
// content padded to 512 bytes.
function readFileFromTarball(tarballPath: string, entryPath: string): string {
  const TAR_BLOCK_SIZE = 512;
  const archive = gunzipSync(readFileSync(tarballPath));
  let offset = 0;
  while (offset + TAR_BLOCK_SIZE <= archive.length) {
    const header = archive.subarray(offset, offset + TAR_BLOCK_SIZE);
    const path = readTarHeaderField(header, 0, 100);
    if (path === "") {
      break;
    }
    const size = parseInt(readTarHeaderField(header, 124, 136), 8);
    const contentStart = offset + TAR_BLOCK_SIZE;
    if (path === entryPath) {
      return archive.toString("utf-8", contentStart, contentStart + size);
    }
    offset = contentStart + Math.ceil(size / TAR_BLOCK_SIZE) * TAR_BLOCK_SIZE;
  }
  throw new Error(`${entryPath} not found in ${tarballPath}`);
}

function readTarHeaderField(
  header: Buffer,
  start: number,
  end: number,
): string {
  return header.toString("utf-8", start, end).replace(/\0.*$/s, "").trim();
}

function haveSameContent(pathA: string, pathB: string): boolean {
  if (!existsSync(pathA) || !existsSync(pathB)) {
    return existsSync(pathA) === existsSync(pathB);
  }
  return readFileSync(pathA, "utf-8") === readFileSync(pathB, "utf-8");
}
