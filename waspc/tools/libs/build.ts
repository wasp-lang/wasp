/// <reference types="node" />
// Helper to compile the waspc/libs/* packages locally and in CI and then move
// them into the Cabal data dir.

import { readdirSync, rmSync } from "node:fs";
import { join } from "node:path";
import {
  assertPackageVersionMatchesWaspc,
  discoverSubDirs,
  getPackageJson,
  runCmd,
} from "../utils.ts";
import { getDataLibsDirPath } from "./utils.ts";

buildLibs();

function buildLibs(): void {
  const dataLibsDirPath = getDataLibsDirPath();
  const libDirs = discoverSubDirs(dataLibsDirPath);

  for (const libDir of libDirs) {
    buildLib(libDir);
  }
}

function buildLib(libDir: string): void {
  const { name: libName, version: libVersion } = getPackageJson(libDir);

  assertPackageVersionMatchesWaspc(libName, libVersion);

  console.log(`Building ${libName} lib (${libDir})`);

  rmExistingTarballsInDir(libDir);

  runCmd("npm", ["install"], { cwd: libDir });
  runCmd("npm", ["pack"], { cwd: libDir });
}

function rmExistingTarballsInDir(dir: string): void {
  const oldTarballs = readdirSync(dir).filter((f) => f.endsWith(".tgz"));
  for (const tarballFileName of oldTarballs) {
    rmSync(join(dir, tarballFileName));
  }
}
