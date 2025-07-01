import { waspSays } from "../../../../common/terminal.js";
import {
  clientTomlExistsInProject,
  getTomlFilePaths,
  serverTomlExistsInProject,
} from "../../tomlFile.js";
import { createDb } from "../createDb/createDb.js";
import { deploy } from "../deploy/deploy.js";
import { setup } from "../setup/setup.js";
import { LaunchCmdOptions } from "./LaunchCmdOptions.js";

export async function launch(
  basename: string,
  region: string,
  cmdOptions: LaunchCmdOptions,
): Promise<void> {
  waspSays("Launching your Wasp app to Fly.io!");

  const tomlFilePaths = getTomlFilePaths(cmdOptions);
  if (
    serverTomlExistsInProject(tomlFilePaths) ||
    clientTomlExistsInProject(tomlFilePaths)
  ) {
    throw new Error(
      "You already have Fly toml files. The launch command is intended to be run one time on a new Fly project. Please try a different command.",
    );
  }

  await setup(basename, region, cmdOptions);

  await createDb(region, cmdOptions);

  await deploy(cmdOptions);
}
