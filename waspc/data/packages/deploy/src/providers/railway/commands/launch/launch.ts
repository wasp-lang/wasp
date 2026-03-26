import { retryOnTransientError } from "../../../../common/retry.js";
import { waspSays } from "../../../../common/terminal.js";
import { RailwayProjectName } from "../../brandedTypes.js";
import { isRailwayTransientError } from "../../transientErrors.js";

import { deploy } from "../deploy/index.js";
import { setup } from "../setup/setup.js";
import { LaunchCmdOptions } from "./LaunchCmdOptions.js";

export async function launch(
  projectName: RailwayProjectName,
  options: LaunchCmdOptions,
): Promise<void> {
  waspSays("Launching your Wasp app to Railway!");

  await retryOnTransientError(() => setup(projectName, options), {
    isRetryable: isRailwayTransientError,
  });

  await retryOnTransientError(() => deploy(projectName, options), {
    isRetryable: isRailwayTransientError,
  });
}
