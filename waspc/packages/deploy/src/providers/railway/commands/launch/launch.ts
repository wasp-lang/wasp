import { waspSays } from "../../../../common/terminal.js";
import { RailwayProjectName } from "../../brandedTypes.js";

import { deploy } from "../deploy/index.js";
import { setup } from "../setup/setup.js";
import { LaunchOptions } from "./LaunchOptions.js";

export async function launch(
  projectName: RailwayProjectName,
  options: LaunchOptions,
): Promise<void> {
  waspSays("Launching your Wasp app to Railway!");

  await setup(projectName, options);

  await deploy(projectName, options);
}
