import { waspSays } from "../../../../common/terminal.js";
import { getServerArtefactsDir } from "../../../../common/waspProject.js";
import { DeploymentInfo } from "../../DeploymentInfo.js";

import {
  deployServiceWithStreamingLogs,
  ServiceDeploymentStatus,
} from "./common.js";
import { DeployOptions } from "./DeployOptions.js";

export async function deployServer({
  options,
  serverServiceName,
}: DeploymentInfo<DeployOptions>): Promise<void> {
  waspSays("Deploying your server now...");

  const serverArtefactsDir = getServerArtefactsDir(options.waspProjectDir);

  const status = await deployServiceWithStreamingLogs(
    {
      name: serverServiceName,
      artefactsDirectory: serverArtefactsDir,
    },
    options,
  );

  const messages: Record<ServiceDeploymentStatus, string> = {
    [ServiceDeploymentStatus.SUCCESS]: "Server has been deployed!",
    [ServiceDeploymentStatus.FAILED_STREAMING_LOGS]:
      "Server deployment started, but failed to stream build logs. Please check the Railway dashboard for build logs.",
  };

  waspSays(messages[status]);
}
