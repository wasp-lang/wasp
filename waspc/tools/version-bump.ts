/// <reference types="node" />

import assert from "node:assert";
import { existsSync, readFileSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import semver from "semver";
import { cabalVersionRegex, getWaspcVersion } from "./get-waspc-version.ts";
import { getLibsDirPath } from "./libs/utils.ts";
import {
  getPackageDirPath,
  WASP_NPM_PACKAGE_NAMES,
  WASPC_PACKAGES,
} from "./packages/utils.ts";
import {
  discoverSubDirs,
  findWaspProjectDirsAbsPathInRepo as findWaspProjectDirsPathsInRepo,
  getWaspcDirPath,
  runCmd,
} from "./utils.ts";

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
  rebuildPackages();
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
  for (const { name } of WASPC_PACKAGES) {
    bumpPackageJsonVersion(getPackageDirPath(name), nextVersion);
  }
  for (const name of WASP_NPM_PACKAGE_NAMES) {
    bumpPackageJsonVersion(getPackageDirPath(name), nextVersion);
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

function bumpWaspProjectVersion(projectDir: string, nextVersion: string): void {
  bumpWaspFileVersion(projectDir, nextVersion);
  bumpDevWaspPackageTarballs(projectDir, nextVersion);
}

function bumpWaspFileVersion(projectDir: string, nextVersion: string): void {
  const waspFilePath = findWaspFilePath(projectDir);
  const waspFileContent = readFileSync(waspFilePath, "utf-8");

  // A bit unstable, but it should be good enough for our "controlled environment".
  const updatedWaspFileContent = waspFileContent.replace(
    /(wasp:\s*\{[^}]*version:\s*)["'`][^"'`]+["'`]/,
    `$1"${nextVersion}"`,
  );
  if (waspFileContent === updatedWaspFileContent) {
    throw new Error(`Failed to update the ${waspFilePath} Wasp version`);
  }

  writeFileSync(waspFilePath, updatedWaspFileContent);
}

// The Wasp projects in this repo depend on the dev Wasp npm package tarballs
// (see `tools/dev-packages/build.ts`), whose file names carry the version.
function bumpDevWaspPackageTarballs(
  projectDir: string,
  nextVersion: string,
): void {
  const packageJsonPath = join(projectDir, "package.json");
  const packageJsonContent = readFileSync(packageJsonPath, "utf-8");

  const devTarballRegex =
    /(file:.*\/wasp-(?:wasp|cli|spec)-)\d+\.\d+\.\d+(\.tgz)/g;
  const updatedPackageJsonContent = packageJsonContent.replace(
    devTarballRegex,
    `$1${nextVersion}$2`,
  );
  if (packageJsonContent === updatedPackageJsonContent) {
    throw new Error(
      `Failed to update the dev Wasp npm package tarballs in ${packageJsonPath}`,
    );
  }

  writeFileSync(packageJsonPath, updatedPackageJsonContent);
}

function findWaspFilePath(projectDir: string): string {
  const path = join(projectDir, "main.wasp.ts");
  if (existsSync(path)) {
    return path;
  }
  throw new Error(`No main.wasp.ts file in ${projectDir}`);
}

function rebuildLibs(): void {
  runCmd("node", [join("tools", "libs", "build.ts")], {
    cwd: waspcDirPath,
    stdio: "inherit",
  });
}

function rebuildPackages(): void {
  runCmd("node", [join("tools", "packages", "build.ts")], {
    cwd: waspcDirPath,
    stdio: "inherit",
  });
}

// The Wasp projects in this repo depend on the dev tarballs by version (see
// `bumpDevWaspPackageTarballs`), so they must exist at the new version before
// the projects are reinstalled below.
function rebuildDevPackages(): void {
  runCmd("node", [join("tools", "dev-packages", "build.ts")], {
    cwd: waspcDirPath,
    stdio: "inherit",
  });
}

function bustWaspProjectsLibsCache(): void {
  const runScriptFilePath = join(waspcDirPath, "run");

  for (const waspProjectDirPath of waspProjectDirsPaths) {
    runCmd(runScriptFilePath, ["bust-libs-cache"], {
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
