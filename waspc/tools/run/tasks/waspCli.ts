/// <reference types="node" />

import { join } from "node:path";
import { getWaspcDirPath } from "../../utils.ts";
import { runCmd } from "../runCmd.ts";

export const waspCliDevPath = join(getWaspcDirPath(), "tools", "wasp-cli-dev");
export const waspCliDevEnv = { WASP_CLI_CMD: waspCliDevPath };

export async function runWaspCli(
  args: string[],
  appDir: string,
): Promise<void> {
  await runCmd(waspCliDevPath, args, { cwd: appDir });
}
