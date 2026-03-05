import { $ } from "zx";

import { waspSays } from "../../../../common/terminal.js";
import { ensureWaspProjectIsBuilt } from "../../../../common/waspBuild.js";
import { CommonOps, getCommonOps } from "../../CommonOps.js";
import {
  deleteLocalToml,
  getTomlFilePaths,
  localTomlExists,
} from "../../tomlFile.js";
import { CmdCmdOptions } from "./CmdCmdOptions.js";

// Runs a command by copying down the project toml files, executing it, and copying it back up (just in case).
// If the toml file does not exist, some commands will not run with additional args (e.g. -a <appname>).
export async function cmd(
  flyctlArgs: string[],
  cmdOptions: CmdCmdOptions,
): Promise<void> {
  if (cmdOptions.org) {
    flyctlArgs.push("--org", cmdOptions.org);
  }

  waspSays(
    `Running ${cmdOptions.context} command: flyctl ${flyctlArgs.join(" ")}`,
  );

  await ensureWaspProjectIsBuilt(cmdOptions);

  const tomlFilePaths = getTomlFilePaths(cmdOptions);
  const commonOps = getCommonOps(
    cmdOptions.context,
    cmdOptions.waspProjectDir,
    tomlFilePaths,
  );

  await runFlyctlCommand(commonOps, flyctlArgs);
}

async function runFlyctlCommand(
  commonOps: CommonOps,
  flyctlArgs: string[],
): Promise<void> {
  commonOps.cdToDeploymentDir();
  deleteLocalToml();
  if (commonOps.tomlExistsInProject()) {
    commonOps.copyProjectTomlLocally();
  }

  await $`flyctl ${flyctlArgs}`;

  if (localTomlExists()) {
    commonOps.copyLocalTomlToProject();
  }
}
