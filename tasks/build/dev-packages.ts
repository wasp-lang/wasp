#!/usr/bin/env node
//MISE description="Packs the unreleased @wasp/wasp and the packages it depends on into waspc/.dev-packages/, for Wasp projects to depend on the Wasp in this repo."
/// <reference types="node" />
// Packs the unreleased `@wasp/wasp` and the packages it depends on
// (`@wasp/cli`, `@wasp/spec`, `@wasp/deploy`, `@wasp/studio`) into tarballs
// under `waspc/.dev-packages/`, so Wasp projects can depend on the Wasp in
// this repo before it is published. `waspc/tools/wasp-cli-dev` points the
// CLI at that directory, and `wasp new` then writes the tarball paths into the
// new project's package.json (see `Wasp.Cli.DevWaspNpmPackages`).
//
// The dev `@wasp/cli` has no platform packages. Its `data.json` points at a
// dev sub-package inside it, which runs the `wasp-cli` binary built in this
// repo. See `makeDevSubPackageSource` for how it finds the binary, and
// `writeDevCliConfig` for the `WASP_DEV_CLI_BIN` and `WASP_DEV_DATA_DIR` overrides.
//
// TODO: Remove this script, or most of it, once Wasp apps can run under pnpm
// and the repo's examples and other Wasp apps join the pnpm workspace. They
// then depend on `@wasp/wasp` through `workspace:*` instead of tarballs. Only
// finding the dev `wasp-cli` binary stays, as a dev fallback in `packages/cli`.

import { mkdirSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { report } from "node:process";
import {
  CLI_PACKAGE_SOURCE_DIR,
  CLI_PACKAGE_STATIC_FILES,
  copyStaticFiles,
} from "../../scripts/make-cli-npm-packages/src/common.ts";
import {
  type CliPackageData,
  UNDEFINED_LIBC_NAME,
} from "../../scripts/make-cli-npm-packages/src/schema/output-data.ts";
import { getPackageDirPath } from "../../waspc/tools/packages/utils.ts";
import {
  getPackageJson,
  getRepoRootPath,
  getWaspcDirPath,
  runCmd,
  runCmdAndGetStdout,
} from "../../waspc/tools/utils.ts";

const DEV_PACKAGES_DIR_NAME = ".dev-packages";
const DEV_CLI_CONFIG_FILE_NAME = "dev-cli.json";
const DEV_SUB_PACKAGE_FILE_NAME = "dev-sub-package.js";

// The packages `@wasp/wasp` depends on, besides `@wasp/cli`.
const WASP_NPM_PACKAGE_DEPENDENCY_DIR_NAMES: string[] = [
  "spec",
  "deploy",
  "studio",
];

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
    packPackage(getPackageDirPath(packageDirName), devPackagesDir);
  }

  writeDevCliConfig(devPackagesDir);
  packDevCliPackage(devPackagesDir);

  packPackage(getPackageDirPath("wasp"), devPackagesDir);

  console.log(`Dev packages are in ${devPackagesDir}`);
}

function packPackage(packageDir: string, destinationDir: string): void {
  const { name } = getPackageJson(packageDir);
  console.log(`Packing ${name}`);
  runCmd("pnpm", ["pack", "--pack-destination", destinationDir], {
    cwd: packageDir,
  });
}

// Where the dev `@wasp/cli` finds the binary and the data dir. It lives next
// to the tarballs, not inside them, so the tarballs have the same content on
// every machine and the lockfiles of the repo's Wasp apps stay stable.
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
  const tmpDir = join(tmpdir(), `wasp-dev-cli-${process.pid}`);
  rmSync(tmpDir, { recursive: true, force: true });
  mkdirSync(tmpDir, { recursive: true });

  // Same files as the published package (see `scripts/make-cli-npm-packages`),
  // plus the dev sub-package and a package.json that lists it.
  copyStaticFiles(CLI_PACKAGE_STATIC_FILES, CLI_PACKAGE_SOURCE_DIR, tmpDir);
  writeFileSync(
    join(tmpDir, DEV_SUB_PACKAGE_FILE_NAME),
    makeDevSubPackageSource(),
  );

  const packageJson = getPackageJson(CLI_PACKAGE_SOURCE_DIR) as {
    files?: string[];
  };
  packageJson.files = [
    ...CLI_PACKAGE_STATIC_FILES,
    "data.json",
    DEV_SUB_PACKAGE_FILE_NAME,
  ];
  writeFileSync(
    join(tmpDir, "package.json"),
    JSON.stringify(packageJson, null, 2) + "\n",
  );

  // Same shape `scripts/make-cli-npm-packages` writes, with the dev sub-package
  // for this machine's platform. `bin.js` passes `packageName` to `import()`,
  // which resolves the relative path against `bin.js`.
  const { os, cpu, libc } = getCurrentTarget();
  const data = {
    subPackages: {
      [os]: {
        [cpu]: { [libc]: { packageName: `./${DEV_SUB_PACKAGE_FILE_NAME}` } },
      },
    },
  } satisfies CliPackageData;
  writeFileSync(join(tmpDir, "data.json"), JSON.stringify(data));

  packPackage(tmpDir, devPackagesDir);
  rmSync(tmpDir, { recursive: true, force: true });
}

// The dev stand-in for the platform packages of `@wasp/cli`. It finds the
// project it is installed in (the one whose package.json points at the dev
// `@wasp/wasp` tarball), and reads the binary and data dir paths from the
// config file next to that tarball. The env vars win when set.
// Its default export must match `SubPackageAPI` in
// `scripts/make-cli-npm-packages/src/schema/output-data.ts`, which `bin.js` reads.
function makeDevSubPackageSource(): string {
  return `\
import { existsSync, readFileSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const DEV_CLI_CONFIG_FILE_NAME = ${JSON.stringify(DEV_CLI_CONFIG_FILE_NAME)};

const devCliConfig = readDevCliConfig();

export default {
  waspBinPath: process.env.WASP_DEV_CLI_BIN ?? devCliConfig.waspBinPath,
  dataDirPath: process.env.WASP_DEV_DATA_DIR ?? devCliConfig.dataDirPath,
};

function readDevCliConfig() {
  const configFile = join(findDevPackagesDir(), DEV_CLI_CONFIG_FILE_NAME);
  return JSON.parse(readFileSync(configFile, "utf8"));
}

function findDevPackagesDir() {
  let dir = dirname(fileURLToPath(import.meta.url));
  while (true) {
    const packageJsonPath = join(dir, "package.json");
    if (existsSync(packageJsonPath)) {
      const packageJson = JSON.parse(readFileSync(packageJsonPath, "utf8"));
      const specifier = packageJson.dependencies?.["@wasp/wasp"];
      if (typeof specifier === "string" && specifier.startsWith("file:")) {
        return dirname(resolve(dir, specifier.slice("file:".length)));
      }
    }
    const parentDir = dirname(dir);
    if (parentDir === dir) {
      throw new Error(
        "This is a development build of @wasp/cli, but no package.json above it depends on the dev @wasp/wasp tarball.",
      );
    }
    dir = parentDir;
  }
}
`;
}

// Keep in sync with the platform detection in `packages/cli/bin.js`.
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
