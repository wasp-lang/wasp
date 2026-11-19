/// <reference types="node" />
// Builds the lib in the current directory and packs it into the Cabal data
// dir, where `waspc` expects it. Run by the libs' `build` mise tasks.

import { mkdirSync, readdirSync, rmSync } from "node:fs";
import { basename, join } from "node:path";
import {
  assertPackageVersionMatchesWaspc,
  getPackageJson,
  runCmd,
} from "../utils.ts";
import { getDataLibsDirPath } from "./utils.ts";

try {
  buildLib(process.cwd());
} catch (e) {
  console.error(`ERROR: ${e instanceof Error ? e.message : String(e)}`);
  process.exitCode = 1;
}

function buildLib(libDir: string): void {
  const { name, version } = getPackageJson(libDir);
  assertPackageVersionMatchesWaspc(name, version);

  console.log(`Building ${name} lib (${libDir})`);
  runCmd("pnpm", ["run", "build"], { cwd: libDir, stdio: "inherit" });

  const dataLibDir = join(getDataLibsDirPath(), basename(libDir));
  console.log(`Packing ${name} lib into ${dataLibDir}`);
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
