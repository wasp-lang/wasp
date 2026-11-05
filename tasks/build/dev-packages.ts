#!/usr/bin/env node
//MISE description="Packs the unreleased @wasp/wasp and the packages it depends on into waspc/.dev-packages/, for Wasp projects to depend on the Wasp in this repo."
/// <reference types="node" />
// The unreleased Wasp isn't on npm, so the repo's Wasp apps depend on tarballs
// of it instead (see `Wasp.Cli.DevWaspNpmPackages`). The dev `@wasp/cli` runs
// the `wasp-cli` binary built in this repo (see `waspc/tools/devCliSubPackage.js`).
//
// TODO: Remove most of this once the repo's Wasp apps join the pnpm workspace
// and depend on `@wasp/wasp` through `workspace:*`.

import {
  copyFileSync,
  mkdirSync,
  mkdtempSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { report } from "node:process";
import {
  CLI_PACKAGE_SOURCE_DIR,
  CLI_PACKAGE_STATIC_FILES,
  copyStaticFiles,
} from "../../packages/cli/scripts/make-cli-npm-packages/src/common.ts";
import {
  type CliPackageData,
  UNDEFINED_LIBC_NAME,
} from "../../packages/cli/scripts/make-cli-npm-packages/src/schema/output-data.ts";
import {
  getPackageDirPath,
  WASP_NPM_PACKAGE_DEPENDENCY_DIR_NAMES,
  WASP_NPM_PACKAGE_DIR_NAME,
} from "../../waspc/tools/packages/utils.ts";
import {
  assertPackageVersionMatchesWaspc,
  getPackageJson,
  getRepoRootPath,
  getWaspcDirPath,
  runCmd,
  runCmdAndGetStdout,
} from "../../waspc/tools/utils.ts";

const DEV_PACKAGES_DIR_NAME = ".dev-packages";
// Keep in sync with `waspc/tools/devCliSubPackage.js`.
const DEV_CLI_CONFIG_FILE_NAME = "dev-cli.json";
const DEV_SUB_PACKAGE_FILE_NAME = "dev-sub-package.js";
const DEV_SUB_PACKAGE_SOURCE_PATH = join(
  getWaspcDirPath(),
  "tools",
  "devCliSubPackage.js",
);

// We pack `@wasp/cli` ourselves, with the dev sub-package.
const CLI_PACKAGE_DIR_NAME = "cli";

try {
  buildDevPackages();
} catch (e) {
  console.error(`ERROR: ${e instanceof Error ? e.message : String(e)}`);
  process.exitCode = 1;
}

function buildDevPackages(): void {
  const devPackagesDir = join(getWaspcDirPath(), DEV_PACKAGES_DIR_NAME);
  rmSync(devPackagesDir, { recursive: true, force: true });
  mkdirSync(devPackagesDir, { recursive: true });

  for (const packageDirName of WASP_NPM_PACKAGE_DEPENDENCY_DIR_NAMES) {
    console.log(`Building @wasp/${packageDirName}`);
    runCmd(
      "pnpm",
      [
        "--filter",
        `@wasp/${packageDirName}`,
        "--fail-if-no-match",
        "run",
        "build",
      ],
      {
        cwd: getRepoRootPath(),
      },
    );
    if (packageDirName !== CLI_PACKAGE_DIR_NAME) {
      packPackage(getPackageDirPath(packageDirName), devPackagesDir);
    }
  }

  writeDevCliConfig(devPackagesDir);
  packDevCliPackage(devPackagesDir);

  packPackage(getPackageDirPath(WASP_NPM_PACKAGE_DIR_NAME), devPackagesDir);

  console.log(`Dev packages are in ${devPackagesDir}`);
}

function packPackage(packageDir: string, destinationDir: string): void {
  const { name, version } = getPackageJson(packageDir);
  // `Wasp.Cli.DevWaspNpmPackages` looks for the tarballs at the Wasp version.
  assertPackageVersionMatchesWaspc(name, version);
  console.log(`Packing ${name}`);
  runCmd("pnpm", ["pack", "--pack-destination", destinationDir], {
    cwd: packageDir,
  });
}

// Next to the tarballs, not inside them, so the tarballs are the same on every
// machine and the lockfiles of the repo's Wasp apps stay stable.
function writeDevCliConfig(devPackagesDir: string): void {
  const waspBinPath = process.env.WASP_DEV_CLI_BIN ?? getCabalWaspCliBinPath();
  const dataDirPath =
    process.env.WASP_DEV_DATA_DIR ?? join(getWaspcDirPath(), "data");

  writeFileSync(
    join(devPackagesDir, DEV_CLI_CONFIG_FILE_NAME),
    JSON.stringify({ waspBinPath, dataDirPath }, null, 2) + "\n",
  );

  console.log(`Dev @wasp/cli runs ${waspBinPath} with data dir ${dataDirPath}`);
}

function getCabalWaspCliBinPath(): string {
  return runCmdAndGetStdout("cabal", ["list-bin", "wasp-cli"], {
    cwd: getWaspcDirPath(),
  }).trim();
}

function packDevCliPackage(devPackagesDir: string): void {
  const tmpDir = mkdtempSync(join(tmpdir(), "wasp-dev-cli-"));
  try {
    // Same files as the published package (see `packages/cli/scripts/make-cli-npm-packages`),
    // plus the dev sub-package next to the compiled CLI, and a package.json
    // that lists them.
    copyStaticFiles(CLI_PACKAGE_STATIC_FILES, CLI_PACKAGE_SOURCE_DIR, tmpDir);
    copyFileSync(
      DEV_SUB_PACKAGE_SOURCE_PATH,
      join(tmpDir, "dist", DEV_SUB_PACKAGE_FILE_NAME),
    );

    const { scripts, devDependencies, ...cliPackageJson } = getPackageJson(
      CLI_PACKAGE_SOURCE_DIR,
    );
    const packageJson = {
      ...cliPackageJson,
      files: [...CLI_PACKAGE_STATIC_FILES, "data.json"],
    };
    writeFileSync(
      join(tmpDir, "package.json"),
      JSON.stringify(packageJson, null, 2) + "\n",
    );

    // Same shape `make-cli-npm-packages` writes, with only this machine's
    // platform. `dist/run.js` imports `packageName`, a path relative to it.
    const { os, cpu, libc } = getCurrentTarget();
    const data = {
      subPackages: {
        [os]: {
          [cpu]: {
            [libc]: { packageName: `./${DEV_SUB_PACKAGE_FILE_NAME}` },
          },
        },
      },
    } satisfies CliPackageData;
    writeFileSync(join(tmpDir, "data.json"), JSON.stringify(data));

    packPackage(tmpDir, devPackagesDir);
  } finally {
    rmSync(tmpDir, { recursive: true, force: true });
  }
}

// Keep in sync with the platform detection in `packages/cli/src/run.ts`.
function getCurrentTarget(): { os: string; cpu: string; libc: string } {
  const { platform: os, arch: cpu } = process;
  const libc =
    os === "linux" ? (isGlibc() ? "glibc" : "musl") : UNDEFINED_LIBC_NAME;
  return { os, cpu, libc };
}

function isGlibc(): boolean {
  try {
    const header = (
      report.getReport() as { header?: { glibcVersionRuntime?: string } }
    ).header;
    return Boolean(header?.glibcVersionRuntime);
  } catch {
    return false;
  }
}
