import { exit } from "process";
import { getCommandName } from "../../../common/commander.js";
import { waspSays } from "../../../common/terminal.js";
import { deploy } from "../deploy/index.js";
import { RailwayProjectName } from "../DeploymentInfo.js";
import { railwayDeployCommand, railwaySetupCommand } from "../index.js";
import { setup } from "../setup/setup.js";
import { LaunchOptions } from "./LaunchOptions.js";

export async function launch(
  projectName: RailwayProjectName,
  options: LaunchOptions,
): Promise<void> {
  waspSays("Launching your Wasp app to Railway!");

  try {
    await setup(projectName, options);
  } catch (e) {
    waspSays(
      `There was an error running "${getCommandName(
        railwaySetupCommand,
      )}". Please review the error and try again (if appropriate).`,
    );
    exit(1);
  }

  try {
    await deploy(projectName, { ...options, skipBuild: true });
  } catch (e) {
    waspSays(
      `There was an error running "${getCommandName(
        railwayDeployCommand,
      )}". Please review the error and try again (if appropriate).`,
    );
    exit(1);
  }
}
