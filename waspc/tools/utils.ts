/// <reference types="node" />

import { execaSync } from "execa";
import { readFileSync } from "node:fs";
import { join } from "node:path";
import { fileURLToPath } from "node:url";
import { getWaspcVersion } from "./get-waspc-version.ts";

export function getRepoRootPath(): string {
  return fileURLToPath(new URL("../..", import.meta.url));
}

export function getWaspcDirPath(): string {
  return fileURLToPath(new URL("..", import.meta.url));
}

export type PackageJson = {
  name: string;
  version: string;
  scripts?: Record<string, string>;
  dependencies?: Record<string, string>;
  peerDependencies?: Record<string, string>;
  peerDependenciesMeta?: Record<string, { optional?: boolean }>;
  devDependencies?: Record<string, string>;
};

export function getPackageJson(dir: string): PackageJson {
  const packageJsonPath = join(dir, "package.json");
  return JSON.parse(readFileSync(packageJsonPath, "utf-8"));
}

export function assertPackageVersionMatchesWaspc(
  packageName: string,
  packageVersion: string,
): void {
  const waspcVersion = getWaspcVersion();

  if (packageVersion !== waspcVersion) {
    throw new Error(
      `${packageName} package version (${packageVersion}) != current Wasp version (${waspcVersion}). ` +
        `Update the package version in package.json to ${waspcVersion}.`,
    );
  }
}

export function runCmd(
  cmd: string,
  args: string[],
  options: { cwd: string; stdio?: "inherit" },
): void {
  execaSync(cmd, args, options);
}

export function runCmdAndGetStdout(
  cmd: string,
  args: string[],
  options: { cwd: string },
): string {
  return execaSync(cmd, args, options).stdout;
}
