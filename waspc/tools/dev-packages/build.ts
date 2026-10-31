/// <reference types="node" />
// Packs the unreleased `@wasp/wasp` and the packages it depends on
// (`@wasp/cli`, `@wasp/spec`) into tarballs
// under `waspc/.dev-packages/`, so Wasp projects can depend on the Wasp in
// this repo before it is published. `tools/wasp-cli-dev` points the
// CLI at that directory, and `wasp new` then writes the tarball paths into the
// new project's package.json (see `Wasp.Project.WaspNpmPackage`).
//
// The dev `@wasp/cli` has no platform packages. Its `data.json` points at a
// "dev sub-package" inside it that runs the binary built in this repo (or the
// one given through `WASP_DEV_CLI_BIN` / `WASP_DEV_DATA_DIR`), see
// `writeDevCliConfig` for how it finds them.

import { mkdirSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { report } from "node:process";
import {
  MAIN_PACKAGE_SOURCE_DIR,
  MAIN_PACKAGE_STATIC_FILES,
  copyStaticFiles,
} from "../../../scripts/make-npm-packages/src/common.ts";
import { getPackageDirPath } from "../packages/utils.ts";
import {
  getPackageJson,
  getRepoRootPath,
  getWaspcDirPath,
  runCmd,
  runCmdAndGetStdout,
} from "../utils.ts";

const DEV_PACKAGES_DIR_NAME = ".dev-packages";
const DEV_CLI_CONFIG_FILE_NAME = "dev-cli.json";
const DEV_SUB_PACKAGE_FILE_NAME = "dev-sub-package.js";

// The dev stand-in for the platform packages of `@wasp/cli`. It finds the
// project it is installed in (the one whose package.json points at the dev
// `@wasp/wasp` tarball), and reads the binary and data dir paths from the
// config file next to that tarball. The env vars win when set.
const DEV_SUB_PACKAGE_SOURCE = `\
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

  console.log("Building @wasp/spec");
  runCmd("pnpm", ["--filter", "@wasp/spec", "run", "build"], {
    cwd: getRepoRootPath(),
  });

  writeDevCliConfig(devPackagesDir);

  packPackage(getPackageDirPath("spec"), devPackagesDir);
  packDevCliPackage(devPackagesDir);
  packPackage(getPackageDirPath("wasp"), devPackagesDir);

  console.log(`Dev packages are in ${devPackagesDir}`);
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

  // Same files as the published package (see `scripts/make-npm-packages`),
  // plus the dev sub-package and a package.json that lists it.
  copyStaticFiles(MAIN_PACKAGE_STATIC_FILES, MAIN_PACKAGE_SOURCE_DIR, tmpDir);
  writeFileSync(
    join(tmpDir, DEV_SUB_PACKAGE_FILE_NAME),
    DEV_SUB_PACKAGE_SOURCE,
  );

  const packageJson = getPackageJson(MAIN_PACKAGE_SOURCE_DIR) as {
    files?: string[];
  };
  packageJson.files = [
    ...MAIN_PACKAGE_STATIC_FILES,
    "data.json",
    DEV_SUB_PACKAGE_FILE_NAME,
  ];
  writeFileSync(
    join(tmpDir, "package.json"),
    JSON.stringify(packageJson, null, 2) + "\n",
  );

  // Same shape `scripts/make-npm-packages` writes, with the dev sub-package
  // for this machine's platform. `bin.js` passes `packageName` to `import()`,
  // which resolves the relative path against `bin.js`.
  const { os, cpu, libc } = getCurrentTarget();
  writeFileSync(
    join(tmpDir, "data.json"),
    JSON.stringify({
      subPackages: {
        [os]: {
          [cpu]: { [libc]: { packageName: `./${DEV_SUB_PACKAGE_FILE_NAME}` } },
        },
      },
    }),
  );

  packPackage(tmpDir, devPackagesDir);
  rmSync(tmpDir, { recursive: true, force: true });
}

function packPackage(packageDir: string, destinationDir: string): void {
  const { name } = getPackageJson(packageDir);
  console.log(`Packing ${name}`);
  runCmd("pnpm", ["pack", "--pack-destination", destinationDir], {
    cwd: packageDir,
  });
}

// Keep in sync with the platform detection in `packages/cli/bin.js`.
function getCurrentTarget(): { os: string; cpu: string; libc: string } {
  const { platform: os, arch: cpu } = process;
  const libc = os === "linux" ? (isGlibc() ? "glibc" : "musl") : "unknown";
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
