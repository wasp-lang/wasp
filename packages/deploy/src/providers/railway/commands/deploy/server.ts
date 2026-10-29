import { waspSays } from "../../../../common/terminal.js";
import { getServerBuildArtefactsDir } from "../../../../common/waspProject.js";
import { DeploymentInstructions } from "../../DeploymentInstructions.js";

import {
  deployServiceWithStreamingLogs,
  ServiceDeploymentStatus,
} from "./common.js";
import { DeployCmdOptions } from "./DeployCmdOptions.js";

export async function deployServer({
  cmdOptions: options,
  serverServiceName,
}: DeploymentInstructions<DeployCmdOptions>): Promise<void> {
  waspSays("Deploying your server now...");

  const serverBuildArtefactsDir = getServerBuildArtefactsDir(
    options.waspProjectDir,
  );

  const deploymentStatus = await deployServiceWithStreamingLogs(
    {
      name: serverServiceName,
      dirToDeploy: serverBuildArtefactsDir,
    },
    options,
  );

  const messages: Record<ServiceDeploymentStatus, string> = {
    [ServiceDeploymentStatus.SUCCESS]: "Server has been deployed!",
    [ServiceDeploymentStatus.FAILED_TO_STREAM_LOGS]:
      "Server deployment started, but failed to stream build logs. Please check the Railway dashboard for build logs.",
  };

  waspSays(messages[deploymentStatus]);
}
