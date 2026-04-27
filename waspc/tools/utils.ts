/// <reference types="node" />

import { execFileSync, type ExecFileSyncOptions } from "node:child_process";
import { readdirSync, readFileSync } from "node:fs";
import { join } from "node:path";
import { fileURLToPath } from "node:url";

export function getRepoRootPath(): string {
  return fileURLToPath(new URL("../..", import.meta.url));
}

export function getWaspcDirPath(): string {
  return fileURLToPath(new URL("..", import.meta.url));
}

export function getWaspcVersion(): string {
  return runCmd("node", [join("tools", "get-waspc-version.ts")], {
    cwd: getWaspcDirPath(),
  }).trim();
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
