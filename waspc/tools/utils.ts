/// <reference types="node" />

import { execFileSync, type ExecFileSyncOptions } from "node:child_process";
import { existsSync, readdirSync, readFileSync } from "node:fs";
import { join } from "node:path";
import { fileURLToPath } from "node:url";

export function getRepoRootPath(): string {
  return fileURLToPath(new URL("../..", import.meta.url));
}

export function getWaspcDirPath(): string {
  return fileURLToPath(new URL("..", import.meta.url));
}

export function discoverSubDirs(baseDirPath: string): string[] {
  return readdirSync(baseDirPath, { withFileTypes: true })
    .filter((dirEntry) => dirEntry.isDirectory())
    .map((dir) => join(baseDirPath, dir.name));
}

export function getPackageJson(dir: string): {
  name: string;
  version: string;
  scripts?: Record<string, string>;
} {
  const packageJsonPath = join(dir, "package.json");
  return JSON.parse(readFileSync(packageJsonPath, "utf-8"));
}

export function runCmd(
  cmd: string,
  args: string[],
  options: ExecFileSyncOptions,
): string {
  return execFileSync(cmd, args, {
    ...options,
    encoding: "utf-8",
    // Required for Windows to find `npm` and `node` binaries.
    shell: true,
  });
}

const STARTING_SEARCH_LOCATIONS = ["examples", "mage"];
const DIRS_TO_SKIP = new Set(["node_modules"]);
export function findWaspProjectDirsAbsPathInRepo(): string[] {
  const repoRootPath = getRepoRootPath();

  return STARTING_SEARCH_LOCATIONS.flatMap((searchRoot) =>
    findWaspProjectDirs(join(repoRootPath, searchRoot)),
  );

  function findWaspProjectDirs(currentDir: string): string[] {
    if (isWaspProjectDir(currentDir)) {
      return [currentDir];
    }

    const possibleWaspProjectDirs = readdirSync(currentDir, {
      withFileTypes: true,
    })
      .filter(
        (entry) =>
          entry.isDirectory() &&
          !entry.name.startsWith(".") &&
          !DIRS_TO_SKIP.has(entry.name),
      )
      .map((subDir) => join(currentDir, subDir.name));

    return possibleWaspProjectDirs.flatMap((possibleWaspProjectDir) =>
      findWaspProjectDirs(possibleWaspProjectDir),
    );
  }
}

const WASP_PROJECT_FILE_NAMES = ["main.wasp", "main.wasp.ts"];
export function isWaspProjectDir(dir: string): boolean {
  return WASP_PROJECT_FILE_NAMES.some((name) => existsSync(join(dir, name)));
}
