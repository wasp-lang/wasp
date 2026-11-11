#!/usr/bin/env node
//MISE description="Packs the unreleased @wasp/wasp and the packages it depends on into waspc/.dev-packages/, for Wasp projects to depend on the Wasp in this repo."
/// <reference types="node" />
// TODO(#2262): Once Wasp works with pnpm, the repo's Wasp apps join the pnpm
// workspace and depend on `@wasp/wasp` through `workspace:*`, so they stop
// needing the tarballs. What stays is for projects outside the workspace
// (`wasp new` with the dev CLI) and for telling `@wasp/cli` where the dev
// binary is.
//
// The unreleased Wasp isn't on npm, so the repo's Wasp apps depend on tarballs
// of it instead (see `Wasp.Cli.DevWaspNpmPackages`). The dev `@wasp/cli` runs
// the `wasp-cli` binary built in this repo (see `waspc/tools/devCliSubPackage.ts`).

import {
  mkdirSync,
  mkdtempSync,
  readFileSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { stripTypeScriptTypes } from "node:module";
import { tmpdir } from "node:os";
import { basename, join } from "node:path";
import {
  CLI_PACKAGE_SOURCE_DIR,
  CLI_PACKAGE_STATIC_FILES,
  copyStaticFiles,
} from "../../packages/cli/scripts/make-cli-npm-packages/src/common.ts";
import {
  makeCliPackageData,
  readCliPackageJsonForPublishing,
} from "../../packages/cli/scripts/make-cli-npm-packages/src/generator/cli-package.ts";
import { getCurrentPlatformTarget } from "../../packages/cli/scripts/make-cli-npm-packages/src/schema/output-data.ts";
import {
  getPackageDirPath,
  getPackageName,
  WASP_MAIN_PACKAGE_DIR_NAME,
  WASP_NPM_SUBPACKAGES_DIR_NAMES,
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
// Keep in sync with `waspc/tools/devCliSubPackage.ts` and
// `devWaspNpmPackagesManifestFile` in `Wasp.Cli.DevWaspNpmPackages`.
const DEV_PACKAGES_MANIFEST_FILE_NAME = "dev-packages.json";
const DEV_CLI_SUB_PACKAGE_FILE_NAME = "dev-sub-package.js";
const DEV_CLI_SUB_PACKAGE_SOURCE_PATH = join(
  getWaspcDirPath(),
  "tools",
  "devCliSubPackage.ts",
);

// We pack `@wasp/cli` ourselves, with the dev CLI sub-package.
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

  const dependencyTarballs: Record<string, string> = {};
  for (const packageDirName of WASP_NPM_SUBPACKAGES_DIR_NAMES) {
    const packageName = getPackageName(packageDirName);
    console.log(`Building ${packageName}`);
    runCmd(
      "pnpm",
      ["--filter", packageName, "--fail-if-no-match", "run", "build"],
      { cwd: getRepoRootPath() },
    );
    dependencyTarballs[packageName] =
      packageDirName === CLI_PACKAGE_DIR_NAME
        ? packDevCliPackage(devPackagesDir)
        : packPackage(getPackageDirPath(packageDirName), devPackagesDir);
  }

  const waspTarball = packPackage(
    getPackageDirPath(WASP_MAIN_PACKAGE_DIR_NAME),
    devPackagesDir,
  );

  writeDevPackagesManifest(devPackagesDir, { waspTarball, dependencyTarballs });

  console.log(`Dev packages are in ${devPackagesDir}`);
}

/** Returns the tarball's file name. */
function packPackage(packageDir: string, destinationDir: string): string {
  const { name, version } = getPackageJson(packageDir);
  // The repo's Wasp apps name the tarballs at the Wasp version in their package.json.
  assertPackageVersionMatchesWaspc(name, version);
  console.log(`Packing ${name}`);
  const packOutput: { filename: string } = JSON.parse(
    runCmdAndGetStdout(
      "pnpm",
      ["pack", "--json", "--pack-destination", destinationDir],
      { cwd: packageDir },
    ),
  );
  return basename(packOutput.filename);
}

// What we packed, for `Wasp.Cli.DevWaspNpmPackages`, and what the dev
// `@wasp/cli` runs, for `waspc/tools/devCliSubPackage.ts`.
//
// Next to the tarballs, not inside them, because the paths are of this
// checkout. The tarballs still differ between platforms (see the `data.json`
// of the dev `@wasp/cli`), so `reinstall-dev-wasp-app-deps` always drops their
// lockfile entries.
function writeDevPackagesManifest(
  devPackagesDir: string,
  tarballs: { waspTarball: string; dependencyTarballs: Record<string, string> },
): void {
  const waspBinPath = process.env.WASP_DEV_CLI_BIN ?? getCabalWaspCliBinPath();
  const dataDirPath =
    process.env.WASP_DEV_DATA_DIR ?? join(getWaspcDirPath(), "data");

  writeFileSync(
    join(devPackagesDir, DEV_PACKAGES_MANIFEST_FILE_NAME),
    JSON.stringify({ waspBinPath, dataDirPath, ...tarballs }, null, 2) + "\n",
  );

  console.log(`Dev @wasp/cli runs ${waspBinPath} with data dir ${dataDirPath}`);
}

function getCabalWaspCliBinPath(): string {
  return runCmdAndGetStdout("cabal", ["list-bin", "wasp-cli"], {
    cwd: getWaspcDirPath(),
  }).trim();
}

/** Returns the tarball's file name. */
function packDevCliPackage(devPackagesDir: string): string {
  const tmpDir = mkdtempSync(join(tmpdir(), "wasp-dev-cli-"));
  try {
    // The published package (see `packages/cli/scripts/make-cli-npm-packages`),
    // with the dev CLI sub-package next to the compiled CLI in place of the
    // platform packages.
    copyStaticFiles(CLI_PACKAGE_STATIC_FILES, CLI_PACKAGE_SOURCE_DIR, tmpDir);
    // Node.js doesn't run TypeScript from `node_modules`, where the dev
    // `@wasp/cli` ends up.
    writeFileSync(
      join(tmpDir, "dist", DEV_CLI_SUB_PACKAGE_FILE_NAME),
      stripTypeScriptTypes(
        readFileSync(DEV_CLI_SUB_PACKAGE_SOURCE_PATH, "utf8"),
      ),
    );

    const packageJson = {
      ...readCliPackageJsonForPublishing(),
      files: [...CLI_PACKAGE_STATIC_FILES, "data.json"],
    };
    writeFileSync(
      join(tmpDir, "package.json"),
      JSON.stringify(packageJson, null, 2) + "\n",
    );

    // Only this machine's platform. `dist/run.js` imports `packageName`, a
    // path relative to it.
    const data = makeCliPackageData([
      {
        packageName: `./${DEV_CLI_SUB_PACKAGE_FILE_NAME}`,
        target: getCurrentPlatformTarget(),
      },
    ]);
    writeFileSync(join(tmpDir, "data.json"), JSON.stringify(data));

    return packPackage(tmpDir, devPackagesDir);
  } finally {
    rmSync(tmpDir, { recursive: true, force: true });
  }
}
