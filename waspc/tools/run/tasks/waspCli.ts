/// <reference types="node" />

import { join } from "node:path";
import { getWaspcDirPath } from "../../utils.ts";
import { runCmd } from "../runCmd.ts";

// A single executable that runs the locally-built CLI via `cabal run`.
// Being a single executable (rather than a multi-word command string) lets it
// be a drop-in replacement for `wasp`, with no shell-quoting surprises.
export const waspCliDevPath = join(getWaspcDirPath(), "tools", "wasp-cli-dev");
export const waspCliDevEnv = { WASP_CLI_CMD: waspCliDevPath };

export async function runWaspCli(
  args: string[],
  appDir: string,
): Promise<void> {
  await runCmd(waspCliDevPath, args, { cwd: appDir });
}
