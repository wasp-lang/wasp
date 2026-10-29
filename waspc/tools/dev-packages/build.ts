/// <reference types="node" />
// Packs the unreleased `@wasp/wasp`, `@wasp/cli` and `@wasp/spec` into
// tarballs under `waspc/.dev-packages/`, so Wasp projects can depend on the
// Wasp in this repo before it is published. `tools/wasp-cli-dev` points the
// CLI at that directory, and `wasp new` then writes the tarball paths into the
// new project's package.json (see `Wasp.Project.WaspNpmPackage`).
//
// The dev `@wasp/cli` has no platform packages. Its `data.json` points at a
// single "dev sub-package" that runs the binary built in this repo (or the
// one given through `WASP_DEV_CLI_BIN` / `WASP_DEV_DATA_DIR`).

import { cpSync, mkdirSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { report } from "node:process";
import { pathToFileURL } from "node:url";
import { getPackageDirPath } from "../packages/utils.ts";
import {
  getPackageJson,
  getRepoRootPath,
  getWaspcDirPath,
  installWorkspaceDependencies,
  runCmd,
} from "../utils.ts";

const DEV_PACKAGES_DIR_NAME = ".dev-packages";
const DEV_SUB_PACKAGE_DIR_NAME = "cli-dev-sub-package";
const CLI_PACKAGE_FILES = [
  "bin.js",
  "CLIError.js",
  "readme.md",
  "package.json",
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

  installWorkspaceDependencies();

  console.log("Building @wasp/spec");
  runCmd("pnpm", ["--filter", "@wasp/spec", "run", "build"], {
    cwd: getRepoRootPath(),
  });

  const devSubPackageMainFile = writeDevSubPackage(devPackagesDir);

  packPackage(getPackageDirPath("spec"), devPackagesDir);
  packDevCliPackage(devSubPackageMainFile, devPackagesDir);
  packPackage(getPackageDirPath("wasp"), devPackagesDir);

  console.log(`Dev packages are in ${devPackagesDir}`);
}

// The module `@wasp/cli`'s `bin.js` imports to find the binary and data dir.
function writeDevSubPackage(devPackagesDir: string): string {
  const waspBinPath = process.env.WASP_DEV_CLI_BIN ?? getCabalWaspCliBinPath();
  const dataDirPath =
    process.env.WASP_DEV_DATA_DIR ?? join(getWaspcDirPath(), "data");

  const devSubPackageDir = join(devPackagesDir, DEV_SUB_PACKAGE_DIR_NAME);
  mkdirSync(devSubPackageDir);
  const mainFile = join(devSubPackageDir, "main.js");
  writeFileSync(
    mainFile,
    `export default ${JSON.stringify({ waspBinPath, dataDirPath }, null, 2)};\n`,
  );

  console.log(`Dev @wasp/cli runs ${waspBinPath} with data dir ${dataDirPath}`);
  return mainFile;
}

function getCabalWaspCliBinPath(): string {
  return runCmd("cabal", ["list-bin", "wasp-cli"], {
    cwd: getWaspcDirPath(),
  }).trim();
}

function packDevCliPackage(
  devSubPackageMainFile: string,
  devPackagesDir: string,
): void {
  const cliPackageDir = getPackageDirPath("cli");
  const tmpDir = join(tmpdir(), `wasp-dev-cli-${process.pid}`);
  rmSync(tmpDir, { recursive: true, force: true });
  mkdirSync(tmpDir, { recursive: true });

  for (const file of CLI_PACKAGE_FILES) {
    cpSync(join(cliPackageDir, file), join(tmpDir, file));
  }
  // Same shape `scripts/make-npm-packages` writes, with the dev sub-package
  // for this machine only. `bin.js` passes `packageName` to `import()`, which
  // accepts a `file:` URL on every platform (a Windows path would not be a
  // valid import specifier).
  const { os, cpu, libc } = getCurrentTarget();
  const devSubPackageMainFileUrl = pathToFileURL(devSubPackageMainFile).href;
  writeFileSync(
    join(tmpDir, "data.json"),
    JSON.stringify({
      subPackages: {
        [os]: { [cpu]: { [libc]: { packageName: devSubPackageMainFileUrl } } },
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
