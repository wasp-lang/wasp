/// <reference types="node" />

import { getWaspcVersion } from "../../get-waspc-version.ts";
import { getRepoRootPath, getWaspcDirPath } from "../../utils.ts";
import { runCmd } from "../runCmd.ts";

const repoRootDir = getRepoRootPath();
const waspcDir = getWaspcDirPath();

// The tools and prettier are workspace projects, so install before using them.
export async function installWorkspace(): Promise<void> {
  await runCmd("pnpm", ["install"], { cwd: repoRootDir });
}

export async function buildPackages(): Promise<void> {
  await runTool("packages/build.ts");
}

export async function buildLibs(): Promise<void> {
  await runTool("libs/build.ts");
}

export async function testPackages(): Promise<void> {
  await runTool("packages/test.ts");
}

export async function testLibs(): Promise<void> {
  await runTool("libs/test.ts");
}

export async function bumpWaspVersion(args: string[]): Promise<void> {
  await runTool("version-bump.ts", args);
}

export function printWaspcVersion(): void {
  process.stdout.write(getWaspcVersion());
}

async function runTool(
  toolPath: string,
  toolArgs: string[] = [],
): Promise<void> {
  await runCmd("node", [`tools/${toolPath}`, ...toolArgs], { cwd: waspcDir });
}
