import { $ } from "zx";

import { waspSays } from "../../../../common/terminal.js";
import { ensureWaspProjectIsBuilt } from "../../../../common/waspProject.js";
import { CommonOps, getCommonOps } from "../../CommonOps.js";
import {
  deleteLocalToml,
  getTomlFilePaths,
  localTomlExists,
} from "../../tomlFile.js";
import { CmdOptions } from "./CmdOptions.js";

// Runs a command by copying down the project toml files, executing it, and copying it back up (just in case).
// If the toml file does not exist, some commands will not run with additional args (e.g. -a <appname>).
export async function cmd(
  flyctlArgs: string[],
  options: CmdOptions,
): Promise<void> {
  if (options.org) {
    flyctlArgs.push("--org", options.org);
  }

  waspSays(
    `Running ${options.context} command: flyctl ${flyctlArgs.join(" ")}`,
  );

  await ensureWaspProjectIsBuilt(options);

  const tomlFilePaths = getTomlFilePaths(options);
  const commonOps = getCommonOps(
    options.context,
    options.waspProjectDir,
    tomlFilePaths,
  );

  await runFlyctlCommand(commonOps, flyctlArgs);
}

async function runFlyctlCommand(
  commonOps: CommonOps,
  flyctlArgs: string[],
): Promise<void> {
  commonOps.cdToBuildDir();
  deleteLocalToml();
  if (commonOps.tomlExistsInProject()) {
    commonOps.copyProjectTomlLocally();
  }

  await $`flyctl ${flyctlArgs}`;

  if (localTomlExists()) {
    commonOps.copyLocalTomlToProject();
  }
}
