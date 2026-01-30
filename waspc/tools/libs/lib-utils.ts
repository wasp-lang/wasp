/// <reference types="node" />

import { execFileSync, type ExecFileSyncOptions } from "node:child_process";
import { readdirSync, readFileSync } from "node:fs";
import { join } from "node:path";

export function discoverLibDirs(dataLibsDirPath: string): string[] {
  return readdirSync(dataLibsDirPath, { withFileTypes: true })
    .filter((dirent) => dirent.isDirectory())
    .map((dirent) => join(dataLibsDirPath, dirent.name));
}

export function getLibPackageJson(libDir: string): {
  name: string;
  version: string;
} {
  const packageJsonPath = join(libDir, "package.json");
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
