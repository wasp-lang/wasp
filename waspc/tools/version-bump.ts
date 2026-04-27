/// <reference types="node" />

import assert from "node:assert";
import { existsSync, readFileSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import {
  discoverSubDirs,
  getRepoRootDirPath,
  getWaspcDirPath,
  getWaspcVersion,
  runCmd,
} from "./utils.ts";

const waspcDir = getWaspcDirPath();
const repoRootDir = getRepoRootDirPath();
const runScriptFile = join(waspcDir, "run");
const waspProjectDirsFromRepoRoot = [
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

versionBump(bumpTypeArg);

function versionBump(bumpType: BumpType): void {
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
  const cabalFilePath = join(waspcDir, "waspc.cabal");
  const cabalFileContent = readFileSync(cabalFilePath, "utf-8");
  const updatedCabalFileContent = cabalFileContent.replace(
    /^version:\s*.+$/m,
    `version: ${nextVersion}`,
  );
  writeFileSync(cabalFilePath, updatedCabalFileContent);
}

function bumpLibsVersion(nextVersion: string): void {
  const libsDir = join(waspcDir, "data", "Generator", "libs");
  for (const libDir of discoverSubDirs(libsDir)) {
    bumpPackageJsonVersion(libDir, nextVersion);
  }
}

function bumpWaspProjectsVersion(nextVersion: string): void {
  for (const projectDirFromRepoRoot of waspProjectDirsFromRepoRoot) {
    const projectDir = join(repoRootDir, projectDirFromRepoRoot);
    bumpWaspProjectVersion(projectDir, nextVersion);
  }
}

function bumpPackageJsonVersion(dir: string, nextVersion: string): void {
  const packageJsonPath = join(dir, "package.json");
  const content = readFileSync(packageJsonPath, "utf-8");
  const updated = content.replace(
    /("version":\s*)"[^"]+"/,
    `$1"${nextVersion}"`,
  );
  writeFileSync(packageJsonPath, updated);
}

function bumpWaspProjectVersion(projectDir: string, nextVersion: string): void {
  const waspFilePath = findWaspFilePath(projectDir);
  const waspFileContent = readFileSync(waspFilePath, "utf-8");
  // Matches the version string inside the `wasp: { ... }` block. The same
  // pattern works for both `main.wasp` (multi-line) and `main.wasp.ts`
  // (inline), since both express it as `wasp: { version: "^X.Y.Z" }`.
  const updatedWaspFileContent = waspFileContent.replace(
    /(wasp:\s*\{[^}]*version:\s*)"[^"]+"/,
    `$1"^${nextVersion}"`,
  );
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
    cwd: waspcDir,
  });
}

function bustWaspProjectsLibsCache(): void {
  for (const projectDirFromRepoRoot of waspProjectDirsFromRepoRoot) {
    runCmd(runScriptFile, ["bust-libs-cache"], {
      cwd: join(repoRootDir, projectDirFromRepoRoot),
    });
  }
}

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
