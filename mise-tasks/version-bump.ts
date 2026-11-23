#!/usr/bin/env node
//MISE description="Bumps the Wasp version across waspc.cabal, libs, and example projects, then rebuilds the libs and the dev Wasp npm packages and busts each project's libs cache."
//USAGE arg "<level>" { choices "major" "minor" "patch" }
/// <reference types="node" />

import assert from "node:assert";
import { readFileSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import semver from "semver";
import {
  cabalVersionRegex,
  getWaspcVersion,
} from "../waspc/tools/get-waspc-version.ts";
import { getLibsDirPath } from "../waspc/tools/libs/utils.ts";
import {
  getPackageDirPath,
  WASP_PACKAGE_DIR_NAMES,
} from "../waspc/tools/packages/utils.ts";
import {
  discoverSubDirs,
  findWaspProjectDirsAbsPathInRepo as findWaspProjectDirsPathsInRepo,
  getWaspcDirPath,
  runCmd,
} from "../waspc/tools/utils.ts";

const waspcDirPath = getWaspcDirPath();
const waspProjectDirsPaths = findWaspProjectDirsPathsInRepo();

type BumpType = "major" | "minor" | "patch";

const [_node, _filename, bumpTypeArg] = process.argv;
assert(isBumpType(bumpTypeArg), "Usage: version-bump <major | minor | patch>");

bumpWaspVersion(bumpTypeArg);

function bumpWaspVersion(bumpType: BumpType): void {
  const currentVersion = getWaspcVersion();
  const nextVersion = bumpVersion(currentVersion, bumpType);
  console.log(`Bumping Wasp version: ${currentVersion} -> ${nextVersion}`);

  bumpWaspcCabalVersion(nextVersion);

  bumpLibsVersion(nextVersion);
  rebuildLibs();

  bumpPackagesVersion(nextVersion);
  rebuildDevPackages();

  bumpWaspProjectsVersion(nextVersion);
  bustWaspProjectsLibsCache();
}

function bumpWaspcCabalVersion(nextVersion: string): void {
  const cabalFilePath = join(waspcDirPath, "waspc.cabal");
  const cabalFileContent = readFileSync(cabalFilePath, "utf-8");

  const updatedCabalFileContent = cabalFileContent.replace(
    cabalVersionRegex,
    `version: ${nextVersion}`,
  );
  if (cabalFileContent === updatedCabalFileContent) {
    throw new Error(`Failed to update the ${cabalFilePath} version`);
  }

  writeFileSync(cabalFilePath, updatedCabalFileContent);
}

function bumpLibsVersion(nextVersion: string): void {
  for (const libDirPath of discoverSubDirs(getLibsDirPath())) {
    bumpPackageJsonVersion(libDirPath, nextVersion);
  }
}

function bumpPackagesVersion(nextVersion: string): void {
  for (const packageDirPath of WASP_PACKAGE_DIR_NAMES.map(getPackageDirPath)) {
    bumpPackageJsonVersion(packageDirPath, nextVersion);
  }
}

function bumpWaspProjectsVersion(nextVersion: string): void {
  for (const waspProjectDirAbsPath of waspProjectDirsPaths) {
    bumpWaspProjectVersion(waspProjectDirAbsPath, nextVersion);
  }
}

function bumpPackageJsonVersion(dir: string, nextVersion: string): void {
  const packageJsonPath = join(dir, "package.json");
  const packageJsonContent = readFileSync(packageJsonPath, "utf-8");
  const packageJson: {
    version?: string;
    dependencies?: Record<string, string>;
    devDependencies?: Record<string, string>;
    peerDependencies?: Record<string, string>;
  } = JSON.parse(packageJsonContent);

  if (!packageJson.version) {
    throw new Error(`Failed to update the ${packageJsonPath} version`);
  }
  packageJson.version = nextVersion;

  // Libs and packages depend on each other (e.g. sdk-core on lib-auth), so we
  // must update version numbers in dependencies as well.
  for (const deps of [
    packageJson.dependencies,
    packageJson.devDependencies,
    packageJson.peerDependencies,
  ]) {
    if (!deps) continue;
    for (const [name, range] of Object.entries(deps)) {
      if (name.startsWith("@wasp/") && !range.startsWith("workspace:")) {
        deps[name] = `^${nextVersion}`;
      }
    }
  }

  writeFileSync(
    packageJsonPath,
    JSON.stringify(packageJson, undefined, 2) + "\n",
  );
}

// TODO(#2262): Once the Wasp projects in this repo join the pnpm workspace,
// they depend on `@wasp/wasp` through `workspace:*`, so this and
// `rebuildDevPackages` go away.
//
// The Wasp projects in this repo depend on the dev Wasp npm package tarballs
// (see `tasks/build/dev-packages.ts`), whose file names carry the version.
function bumpWaspProjectVersion(projectDir: string, nextVersion: string): void {
  const packageJsonPath = join(projectDir, "package.json");
  const packageJsonContent = readFileSync(packageJsonPath, "utf-8");

  // Matches every tarball in `.dev-packages/`, named `wasp-<package>-<version>.tgz`.
  const devTarballRegex =
    /(file:[^"]*\/\.dev-packages\/wasp-[a-z-]+-)\d+\.\d+\.\d+(\.tgz)/g;
  const updatedPackageJsonContent = packageJsonContent.replace(
    devTarballRegex,
    `$1${nextVersion}$2`,
  );
  if (packageJsonContent === updatedPackageJsonContent) {
    throw new Error(
      `Failed to update the dev Wasp npm package tarball paths in ${packageJsonPath}`,
    );
  }

  writeFileSync(packageJsonPath, updatedPackageJsonContent);
}

function rebuildLibs(): void {
  runCmd("mise", ["run", "build:libs"], {
    cwd: waspcDirPath,
    stdio: "inherit",
  });
}

// The Wasp projects in this repo depend on the dev tarballs by version (see
// `bumpWaspProjectVersion`), so the tarballs must exist at the new version
// before `bustWaspProjectsLibsCache` reinstalls the projects.
function rebuildDevPackages(): void {
  runCmd("mise", ["run", "build:dev-packages"], {
    cwd: waspcDirPath,
    stdio: "inherit",
  });
}

function bustWaspProjectsLibsCache(): void {
  for (const waspProjectDirPath of waspProjectDirsPaths) {
    runCmd("mise", ["run", "bust-libs-cache"], {
      cwd: waspProjectDirPath,
      stdio: "inherit",
    });
  }
}

function bumpVersion(version: string, bumpType: BumpType): string {
  const nextVersion = semver.inc(version, bumpType);
  if (nextVersion === null) {
    throw new Error(`Invalid version format: ${version}`);
  }
  return nextVersion;
}

function isBumpType(value: string | undefined): value is BumpType {
  return value === "major" || value === "minor" || value === "patch";
}
