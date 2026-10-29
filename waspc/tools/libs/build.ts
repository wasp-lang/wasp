/// <reference types="node" />
// Helper to compile the packages/libs/* packages locally and in CI and pack
// them into tarballs inside the Cabal data dir, where `waspc` expects them.

import { mkdirSync, readdirSync, rmSync } from "node:fs";
import { basename, join } from "node:path";
import {
  assertPackageVersionMatchesWaspc,
  discoverSubDirs,
  getPackageJson,
  installWorkspaceDependencies,
  runCmd,
} from "../utils.ts";
import {
  getDataLibsDirPath,
  getLibsDirPath,
  runScriptInEveryLib,
} from "./utils.ts";

try {
  buildLibs();
} catch (e) {
  console.error(`ERROR: ${e instanceof Error ? e.message : String(e)}`);
  process.exitCode = 1;
}

function buildLibs(): void {
  installWorkspaceDependencies();

  const libDirs = discoverSubDirs(getLibsDirPath());
  for (const libDir of libDirs) {
    const { name, version } = getPackageJson(libDir);
    assertPackageVersionMatchesWaspc(name, version);
  }

  console.log("Building libs");
  runScriptInEveryLib("build");

  for (const libDir of libDirs) {
    packLib(libDir);
  }
}

function packLib(libDir: string): void {
  const libName = getPackageJson(libDir).name;
  const dataLibDir = join(getDataLibsDirPath(), basename(libDir));

  console.log(`Packing ${libName} lib into ${dataLibDir}`);

  mkdirSync(dataLibDir, { recursive: true });
  rmExistingTarballsInDir(dataLibDir);
  runCmd("pnpm", ["pack", "--pack-destination", dataLibDir], { cwd: libDir });
}

function rmExistingTarballsInDir(dir: string): void {
  const oldTarballs = readdirSync(dir).filter((f) => f.endsWith(".tgz"));
  for (const tarballFileName of oldTarballs) {
    rmSync(join(dir, tarballFileName));
  }
}
