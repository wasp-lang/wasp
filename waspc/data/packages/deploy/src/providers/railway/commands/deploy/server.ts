import {
  displayWaspRocketImage,
  waspSays,
} from "../../../../common/terminal.js";
import { getServerBuildArtefactsDir } from "../../../../common/waspProject.js";
import { DeploymentInstructions } from "../../DeploymentInstructions.js";
import { serverAppPort } from "../../ports.js";
import { generateServiceUrl } from "../../railwayService/url.js";

import {
  deployServiceWithStreamingLogs,
  ServiceDeploymentStatus,
} from "./common.js";
import { DeployCmdOptions } from "./DeployCmdOptions.js";

export async function deployServer({
  cmdOptions: options,
  serverServiceName,
}: DeploymentInstructions<DeployCmdOptions>): Promise<void> {
  waspSays("Deploying your app now...");

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

  displayWaspRocketImage();

  const appUrl = await generateServiceUrl(
    serverServiceName,
    serverAppPort,
    options,
  );
  const messages: Record<ServiceDeploymentStatus, string> = {
    [ServiceDeploymentStatus.SUCCESS]: `Your app has been deployed! It is accessible at: ${appUrl}`,
    [ServiceDeploymentStatus.FAILED_TO_STREAM_LOGS]: `Deployment started, but failed to stream build logs. Your app should be accessible at: ${appUrl}`,
  };

  waspSays(messages[deploymentStatus]);
}
