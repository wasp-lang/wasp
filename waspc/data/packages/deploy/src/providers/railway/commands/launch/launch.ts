import { waspSays } from "../../../../common/terminal.js";
import { RailwayProjectName } from "../../brandedTypes.js";
import { retryOnRailwayAPIError } from "../../retry.js";

import { deploy } from "../deploy/index.js";
import { setup } from "../setup/setup.js";
import { LaunchCmdOptions } from "./LaunchCmdOptions.js";

export async function launch(
  projectName: RailwayProjectName,
  options: LaunchCmdOptions,
): Promise<void> {
  waspSays("Launching your Wasp app to Railway!");

  await retryOnRailwayAPIError(() => setup(projectName, options));

  await retryOnRailwayAPIError(() => deploy(projectName, options));
}
