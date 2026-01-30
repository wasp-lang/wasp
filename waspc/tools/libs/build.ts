/// <reference types="node" />
// Helper to compile the waspc/libs/* packages locally and in CI and then move
// them into the Cabal data dir.

import { readdirSync, rmSync } from "node:fs";
import { join } from "node:path";
import { fileURLToPath } from "node:url";
import { discoverLibDirs, getLibPackageJson, runCmd } from "./lib-utils.ts";

const waspcDirPath = fileURLToPath(new URL("../..", import.meta.url));
const dataLibsDirPath = join(waspcDirPath, "data", "Generator", "libs");
const waspcVersion = getWaspcVersion();

buildLibs();

function buildLibs(): void {
  const libDirs = discoverLibDirs(dataLibsDirPath);

  for (const libDir of libDirs) {
    buildLib(libDir);
  }
}

function buildLib(libDir: string): void {
  const { name: libName, version: libVersion } = getLibPackageJson(libDir);

  assertLibVersionValid(libName, libVersion);

  console.log(`Building ${libName} lib (${libDir})`);

  rmExistingTarballsInDir(libDir);

  runCmd("npm", ["install"], { cwd: libDir });
  runCmd("npm", ["pack"], { cwd: libDir });
}

function assertLibVersionValid(libName: string, libVersion: string): void {
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

function getWaspcVersion(): string {
  return runCmd(
    "node",
    ["--experimental-strip-types", join("tools", "get-waspc-version.ts")],
    { cwd: waspcDirPath },
  ).trim();
}

function rmExistingTarballsInDir(dir: string): void {
  const oldTarballs = readdirSync(dir).filter((f) => f.endsWith(".tgz"));
  for (const tarballFileName of oldTarballs) {
    rmSync(join(dir, tarballFileName));
  }
}
