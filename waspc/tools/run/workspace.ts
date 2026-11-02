/// <reference types="node" />

import { runInRepoRoot, runInWaspc } from "./runCmd.ts";

// The tools and prettier are workspace projects, so install before using them.
export async function installWorkspace(): Promise<void> {
  await runInRepoRoot("pnpm", ["install"]);
}

export async function runNodeTool(
  toolPath: string,
  toolArgs: string[] = [],
): Promise<void> {
  await installWorkspace();
  await runInWaspc("node", [`tools/${toolPath}`, ...toolArgs]);
}
