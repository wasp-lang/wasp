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
import { getDataLibsDirPath, getLibsDirPath } from "./utils.ts";

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
    assertPackageVersionMatchesWaspc(...getNameAndVersion(libDir));
  }

  // One pnpm invocation builds the libs in dependency order.
  console.log("Building libs");
  runCmd("pnpm", ["--filter", "./packages/libs/*", "run", "build"], {
    cwd: getLibsDirPath(),
    stdio: "inherit",
  });

  for (const libDir of libDirs) {
    packLib(libDir);
  }
}

function packLib(libDir: string): void {
  const [libName] = getNameAndVersion(libDir);
  const dataLibDir = join(getDataLibsDirPath(), basename(libDir));

  console.log(`Packing ${libName} lib into ${dataLibDir}`);

  mkdirSync(dataLibDir, { recursive: true });
  rmExistingTarballsInDir(dataLibDir);
  runCmd("pnpm", ["pack", "--pack-destination", dataLibDir], { cwd: libDir });
}

function getNameAndVersion(dir: string): [string, string] {
  const { name, version } = getPackageJson(dir);
  return [name, version];
}

function rmExistingTarballsInDir(dir: string): void {
  const oldTarballs = readdirSync(dir).filter((f) => f.endsWith(".tgz"));
  for (const tarballFileName of oldTarballs) {
    rmSync(join(dir, tarballFileName));
  }
}
