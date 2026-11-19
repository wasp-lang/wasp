#!/usr/bin/env node
//MISE description="Bumps the Wasp version across waspc.cabal and the Wasp npm packages, then rebuilds the dev Wasp npm packages."
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
import {
  getPackageDirPath,
  WASP_PACKAGE_NAMES,
} from "../waspc/tools/packages/utils.ts";
import { getWaspcDirPath, runCmd } from "../waspc/tools/utils.ts";

const waspcDirPath = getWaspcDirPath();

type BumpType = "major" | "minor" | "patch";

const [_node, _filename, bumpTypeArg] = process.argv;
assert(isBumpType(bumpTypeArg), "Usage: version-bump <major | minor | patch>");

bumpWaspVersion(bumpTypeArg);

function bumpWaspVersion(bumpType: BumpType): void {
  const currentVersion = getWaspcVersion();
  const nextVersion = bumpVersion(currentVersion, bumpType);
  console.log(`Bumping Wasp version: ${currentVersion} -> ${nextVersion}`);

  bumpWaspcCabalVersion(nextVersion);

  bumpPackagesVersion(nextVersion);
  // The repo's Wasp apps depend on the packages through `workspace:*`, and
  // the dev tarballs for projects outside the repo carry the version.
  rebuildDevPackages();
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

function bumpPackagesVersion(nextVersion: string): void {
  for (const packageDirPath of WASP_PACKAGE_NAMES.map(getPackageDirPath)) {
    bumpPackageJsonVersion(packageDirPath, nextVersion);
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

  // The packages depend on each other (e.g. `@wasp/sdk-core` on `@wasp/auth`), so we must
  // update version numbers in dependencies as well.
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

function rebuildDevPackages(): void {
  runCmd("mise", ["run", "build:dev-packages"], {
    cwd: waspcDirPath,
    stdio: "inherit",
  });
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
