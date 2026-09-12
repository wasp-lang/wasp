import {
  displayWaspRocketImage,
  waspSays,
} from "../../../../common/terminal.js";
import {
  DeploymentMode,
  getServerBuildArtefactsDir,
} from "../../../../common/waspProject.js";
import { ServerServiceName } from "../../brandedTypes.js";
import { DeploymentInstructions } from "../../DeploymentInstructions.js";
import { serverAppPort } from "../../ports.js";
import { generateServiceUrl } from "../../railwayService/url.js";

import {
  deployServiceWithStreamingLogs,
  ServiceDeploymentStatus,
} from "./common.js";
import { DeployCmdOptions } from "./DeployCmdOptions.js";

export async function deployServer(
  {
    cmdOptions: options,
    serverServiceName,
  }: DeploymentInstructions<DeployCmdOptions>,
  deploymentMode: DeploymentMode,
): Promise<void> {
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

  const appUrlNote = await makeAppUrlNote(
    deploymentMode,
    serverServiceName,
    options,
  );
  const messages: Record<ServiceDeploymentStatus, string> = {
    [ServiceDeploymentStatus.SUCCESS]: `Server has been deployed!${appUrlNote}`,
    [ServiceDeploymentStatus.FAILED_TO_STREAM_LOGS]: `Server deployment started, but failed to stream build logs. Please check the Railway dashboard for build logs.${appUrlNote}`,
  };

  if (deploymentMode === "single") {
    displayWaspRocketImage();
  }
  waspSays(messages[deploymentStatus]);
}

// In single deployment mode the server service serves the web client too, so its
// URL is the app URL.
async function makeAppUrlNote(
  deploymentMode: DeploymentMode,
  serverServiceName: ServerServiceName,
  options: DeployCmdOptions,
): Promise<string> {
  if (deploymentMode !== "single") {
    return "";
  }
  const serverUrl = await generateServiceUrl(
    serverServiceName,
    serverAppPort,
    options,
  );
  return ` Your Wasp app is accessible at: ${serverUrl}`;
}
