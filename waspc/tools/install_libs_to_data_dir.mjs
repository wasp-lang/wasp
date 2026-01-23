// @ts-check

// Helper to compile the waspc/libs/* packages locally and in CI and then move
// them into the Cabal data dir.

import { execFileSync } from "node:child_process";
import {
  copyFileSync,
  existsSync,
  mkdirSync,
  readdirSync,
  readFileSync,
  rmSync,
} from "node:fs";
import { join } from "node:path";
import { fileURLToPath } from "node:url";

const waspcDirPath = fileURLToPath(new URL("..", import.meta.url));
const dataLibsDirPath = join(waspcDirPath, "data", "Generator", "libs");
const waspcVersion = getWaspcVersion();

main();

function main() {
  cleanupOldLibs();
  buildAndCopyLibs();
}

function cleanupOldLibs() {
  if (existsSync(dataLibsDirPath)) {
    rmSync(dataLibsDirPath, { recursive: true, force: true });
  }
  mkdirSync(dataLibsDirPath, { recursive: true });
}

function buildAndCopyLibs() {
  const libsDirPath = join(waspcDirPath, "libs");
  const libDirs = readdirSync(libsDirPath, { withFileTypes: true })
    .filter((dirent) => dirent.isDirectory())
    .map((dirent) => join(libsDirPath, dirent.name));

  for (const libDir of libDirs) {
    buildAndCopyLib(libDir);
  }
}

function buildAndCopyLib(/** @type {string} */ libDir) {
  const { name: libName, version: libVersion } = getLibPackageJson(libDir);

  assertLibVersionValid(libName, libVersion);

  console.log(`Building ${libName} lib (${libDir})`);

  runCmd("npm", ["install"], { cwd: libDir });

  const oldTarballs = getTarballsInDir(libDir);
  for (const tarballFileName of oldTarballs) {
    rmSync(join(libDir, tarballFileName));
  }

  runCmd("npm", ["pack"], { cwd: libDir });

  const newTarballs = getTarballsInDir(libDir);
  for (const tarballFileName of newTarballs) {
    copyFileSync(
      join(libDir, tarballFileName),
      join(dataLibsDirPath, tarballFileName),
    );
  }
}

function getLibPackageJson(/** @type {string} */ libDir) {
  const packageJsonPath = join(libDir, "package.json");
  return JSON.parse(readFileSync(packageJsonPath, "utf-8"));
}

function assertLibVersionValid(
  /** @type {string} */ libName,
  /** @type {string} */ libVersion,
) {
  if (libVersion !== waspcVersion) {
    console.error(
      `ERROR: ${libName} lib version (${libVersion}) != current Wasp version (${waspcVersion}).`,
    );
    console.error(
      `       Update the lib version in package.json to ${waspcVersion}.`,
    );
    throw new Error();
  }
}

function getTarballsInDir(/** @type {string} */ dir) {
  return readdirSync(dir).filter((f) => f.endsWith(".tgz"));
}

function getWaspcVersion() {
  return runCmd("node", [join("tools", "get-waspc-version.mjs")], {
    cwd: waspcDirPath,
  }).trim();
}

function runCmd(
  /** @type {string} */ cmd,
  /** @type {string[]} */ args,
  /** @type {{ cwd: string }} */ { cwd },
) {
  return execFileSync(cmd, args, {
    cwd,
    encoding: "utf-8",
    // Required for Windows to find `npm` and `node`.
    shell: true,
  });
}
