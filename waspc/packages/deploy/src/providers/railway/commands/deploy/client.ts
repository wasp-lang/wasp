import { buildClient } from "../../../../common/clientApp.js";
import {
  displayWaspRocketImage,
  waspSays,
} from "../../../../common/terminal.js";
import { getClientBuildArtefactsDir } from "../../../../common/waspProject.js";
import { DeploymentInstructions } from "../../DeploymentInstructions.js";
import { clientAppPort, serverAppPort } from "../../ports.js";
import { generateServiceUrl } from "../../railwayService/url.js";
import { DeployCmdOptions } from "./DeployCmdOptions.js";
import {
  deployServiceWithStreamingLogs,
  ServiceDeploymentStatus,
} from "./common.js";

export async function deployClient({
  cmdOptions: options,
  serverServiceName,
  clientServiceName,
}: DeploymentInstructions<DeployCmdOptions>): Promise<void> {
  waspSays("Deploying your client now...");

  const serverServiceUrl = await generateServiceUrl(
    serverServiceName,
    serverAppPort,
    options,
  );
  await buildClient(serverServiceUrl, options);

  const webAppArtefactsDir = getClientBuildArtefactsDir(options.waspProjectDir);

  const deploymentStatus = await deployServiceWithStreamingLogs(
    {
      name: clientServiceName,
      artefactsDirectory: webAppArtefactsDir,
    },
    options,
  );

  const clientUrl = await generateServiceUrl(
    clientServiceName,
    clientAppPort,
    options,
  );

  displayWaspRocketImage();

  const messages: Record<ServiceDeploymentStatus, string> = {
    [ServiceDeploymentStatus.SUCCESS]: `Client has been deployed! Your Wasp app is accessible at: ${clientUrl}`,
    [ServiceDeploymentStatus.FAILED_STREAMING_LOGS]: `Client deployment started, but failed to stream build logs. Your Wasp app should be accessible at: ${clientUrl}`,
  };
  waspSays(messages[deploymentStatus]);
}
