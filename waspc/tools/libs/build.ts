/// <reference types="node" />
// Helper to compile the packages/libs/* packages locally and in CI and pack
// them into tarballs inside the Cabal data dir, where `waspc` expects them.

import { mkdirSync, readdirSync, rmSync } from "node:fs";
import { basename, join } from "node:path";
import {
  assertPackageVersionMatchesWaspc,
  discoverSubDirs,
  getPackageJson,
  runCmd,
} from "../utils.ts";
import { getDataLibsDirPath, getLibsDirPath } from "./utils.ts";

try {
  buildLibs();
} catch (e) {
  console.error(`ERROR: ${e instanceof Error ? e.message : String(e)}`);
  process.exitCode = 1;
}

function buildLibs(): void {
  for (const libDir of discoverSubDirs(getLibsDirPath())) {
    buildLib(libDir);
  }
}

function buildLib(libDir: string): void {
  const { name: libName, version: libVersion } = getPackageJson(libDir);

  assertPackageVersionMatchesWaspc(libName, libVersion);

  console.log(`Building ${libName} lib (${libDir})`);

  const dataLibDir = join(getDataLibsDirPath(), basename(libDir));
  mkdirSync(dataLibDir, { recursive: true });
  rmExistingTarballsInDir(dataLibDir);

  runCmd("npm", ["install"], { cwd: libDir });
  runCmd("npm", ["pack", "--pack-destination", dataLibDir], { cwd: libDir });
}

function rmExistingTarballsInDir(dir: string): void {
  const oldTarballs = readdirSync(dir).filter((f) => f.endsWith(".tgz"));
  for (const tarballFileName of oldTarballs) {
    rmSync(join(dir, tarballFileName));
  }
}
