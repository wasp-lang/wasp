/// <reference types="node" />
// Helper to compile the waspc/libs/* packages locally and in CI and then move
// them into the Cabal data dir.

import { execFileSync } from "node:child_process";
import { readdirSync, readFileSync, rmSync } from "node:fs";
import { join } from "node:path";
import { fileURLToPath } from "node:url";

const waspcDirPath = fileURLToPath(new URL("..", import.meta.url));
const dataLibsDirPath = join(waspcDirPath, "data", "Generator", "libs");
const waspcVersion = getWaspcVersion();

buildLibs();

function buildLibs(): void {
  const libDirs = readdirSync(dataLibsDirPath, { withFileTypes: true })
    .filter((dirent) => dirent.isDirectory())
    .map((dirent) => join(dataLibsDirPath, dirent.name));

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

function getLibPackageJson(libDir: string): { name: string; version: string } {
  const packageJsonPath = join(libDir, "package.json");
  return JSON.parse(readFileSync(packageJsonPath, "utf-8"));
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

function runCmd(cmd: string, args: string[], { cwd }: { cwd: string }): string {
  return execFileSync(cmd, args, {
    cwd,
    encoding: "utf-8",
    // Required for Windows to find `npm` and `node` binaries.
    shell: true,
  });
}
