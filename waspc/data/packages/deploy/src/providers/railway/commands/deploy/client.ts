import {
  buildClient,
  isSsrEnabled,
} from "../../../../common/clientApp.js";
import {
  displayWaspRocketImage,
  waspSays,
} from "../../../../common/terminal.js";
import { getClientDeploymentDir } from "../../../../common/waspProject.js";
import { DeploymentInstructions } from "../../DeploymentInstructions.js";
import {
  clientAppPortSsr,
  clientAppPortStatic,
  serverAppPort,
} from "../../ports.js";
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

  const serverServiceUrl =
    options.customServerUrl ??
    (await generateServiceUrl(serverServiceName, serverAppPort, options));

  const clientBuildArtefactsDir = await buildClient(serverServiceUrl, options);

  // Check if SSR is enabled to determine deployment strategy
  const ssrEnabled = isSsrEnabled(options.waspProjectDir);
  const clientAppPort = ssrEnabled ? clientAppPortSsr : clientAppPortStatic;

  // For SSR, we need to deploy the entire web-app directory (not just build artifacts)
  // as it contains the Node.js server and dependencies
  const dirToDeploy = ssrEnabled
    ? getClientDeploymentDir(options.waspProjectDir)
    : clientBuildArtefactsDir;

  if (ssrEnabled) {
    waspSays("SSR is enabled. Deploying as Node.js server...");
  }

  const deploymentStatus = await deployServiceWithStreamingLogs(
    {
      name: clientServiceName,
      dirToDeploy,
    },
    options,
  );

  displayWaspRocketImage();

  const clientUrl = await generateServiceUrl(
    clientServiceName,
    clientAppPort,
    options,
  );
  const messages: Record<ServiceDeploymentStatus, string> = {
    [ServiceDeploymentStatus.SUCCESS]: `Client has been deployed! Your Wasp app is accessible at: ${clientUrl}`,
    [ServiceDeploymentStatus.FAILED_TO_STREAM_LOGS]: `Client deployment started, but failed to stream build logs. Your Wasp app should be accessible at: ${clientUrl}`,
  };
  waspSays(messages[deploymentStatus]);
}
