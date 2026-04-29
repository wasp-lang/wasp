/// <reference types="node" />

import assert from "node:assert";
import { existsSync, readFileSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import { getDataLibsDirPath } from "./libs/utils.ts";
import {
  discoverSubDirs,
  getRepoRootPath,
  getWaspcDirPath,
  getWaspcVersion,
  runCmd,
} from "./utils.ts";

const waspcDirPath = getWaspcDirPath();
const repoRootDirPath = getRepoRootPath();
const runScriptFilePath = join(waspcDirPath, "run");
const waspProjectDirsPathFromRepoRoot = [
  "examples/tutorials/TodoApp",
  "examples/tutorials/TodoAppTs",
  "examples/waspello",
  "examples/waspleau",
  "examples/websockets-realtime-voting",
  "examples/ask-the-documents",
  "examples/kitchen-sink",
  "mage",
];

type BumpType = "major" | "minor" | "patch";

const [_node, _filename, bumpTypeArg] = process.argv;
assert(isBumpType(bumpTypeArg), "Usage: version-bump <major | minor | patch>");

bumpWaspVersion(bumpTypeArg);

function bumpWaspVersion(bumpType: BumpType): void {
  const currentVersion = getWaspcVersion();
  const nextVersion = bumpVersion(currentVersion, bumpType);
  console.log(`Bumping Wasp version: ${currentVersion} -> ${nextVersion}`);

  // Bumping versions
  bumpWaspcCabalVersion(nextVersion);
  bumpLibsVersion(nextVersion);
  bumpWaspProjectsVersion(nextVersion);

  // Busting old libs cache
  rebuildLibs();
  bustWaspProjectsLibsCache();
}

function bumpWaspcCabalVersion(nextVersion: string): void {
  const cabalFilePath = join(waspcDirPath, "waspc.cabal");
  const cabalFileContent = readFileSync(cabalFilePath, "utf-8");

  const updatedCabalFileContent = cabalFileContent.replace(
    /^version:\s*.+$/m,
    `version: ${nextVersion}`,
  );
  if (cabalFileContent === updatedCabalFileContent) {
    throw new Error(`Failed to update the ${cabalFilePath} version`);
  }

  writeFileSync(cabalFilePath, updatedCabalFileContent);
}

function bumpLibsVersion(nextVersion: string): void {
  const dataLibsDirPath = getDataLibsDirPath();
  for (const libDirPath of discoverSubDirs(dataLibsDirPath)) {
    bumpPackageJsonVersion(libDirPath, nextVersion);
  }
}

function bumpWaspProjectsVersion(nextVersion: string): void {
  for (const projectDirPathFromRepoRoot of waspProjectDirsPathFromRepoRoot) {
    const projectDirPath = join(repoRootDirPath, projectDirPathFromRepoRoot);
    bumpWaspProjectVersion(projectDirPath, nextVersion);
  }
}

function bumpPackageJsonVersion(dir: string, nextVersion: string): void {
  const packageJsonPath = join(dir, "package.json");
  const packageJsonContent = readFileSync(packageJsonPath, "utf-8");
  const packageJson: { version?: string } = JSON.parse(packageJsonContent);

  if (!packageJson.version) {
    throw new Error(`Failed to update the ${packageJsonPath} version`);
  }
  packageJson.version = nextVersion;

  writeFileSync(packageJsonPath, JSON.stringify(packageJson));
}

function bumpWaspProjectVersion(projectDir: string, nextVersion: string): void {
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

function findWaspFilePath(projectDir: string): string {
  for (const fileName of ["main.wasp.ts", "main.wasp"]) {
    const path = join(projectDir, fileName);
    if (existsSync(path)) {
      return path;
    }
  }
  throw new Error(`No main.wasp or main.wasp.ts file in ${projectDir}`);
}

function rebuildLibs(): void {
  runCmd("node", [join("tools", "libs", "build.ts")], {
    cwd: waspcDirPath,
    stdio: "inherit",
  });
}

function bustWaspProjectsLibsCache(): void {
  for (const projectDirFromRepoRoot of waspProjectDirsPathFromRepoRoot) {
    runCmd(runScriptFilePath, ["bust-libs-cache"], {
      cwd: join(repoRootDirPath, projectDirFromRepoRoot),
      stdio: "inherit",
    });
  }
}

// TODO: Consider using `semver` package in future.
// So far the `tools` project only has dev dependencies.
// Adding runtime dependencies would change the workflow.
function bumpVersion(version: string, bumpType: BumpType): string {
  const match = version.match(/^(\d+)\.(\d+)\.(\d+)$/);
  if (!match) {
    throw new Error(`Invalid version format: ${version}`);
  }
  const major = Number(match[1]);
  const minor = Number(match[2]);
  const patch = Number(match[3]);
  switch (bumpType) {
    case "major":
      return `${major + 1}.0.0`;
    case "minor":
      return `${major}.${minor + 1}.0`;
    case "patch":
      return `${major}.${minor}.${patch + 1}`;
  }
}

function isBumpType(value: string | undefined): value is BumpType {
  return value === "major" || value === "minor" || value === "patch";
}
