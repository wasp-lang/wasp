import { createCommandWithDirectory } from "../../../common/cli.js";
import {
  getServerUrlFromEnv,
  serverUrlEnvVarName,
} from "../../../common/clientApp.js";
import { displayWaspRocketImage, waspSays } from "../../../common/output.js";
import {
  getClientBuildDir,
  getWebAppArtefactsDir,
} from "../../../common/waspProject.js";
import { DeploymentInfo, ServerServiceName } from "../DeploymentInfo.js";
import { clientAppPort, serverAppPort } from "../helpers/ports.js";
import { generateServiceUrl } from "../helpers/serviceUrl.js";
import { DeployOptions } from "./DeployOptions.js";
import {
  deployServiceWithStreamingLogs,
  ServiceDeploymentStatus,
} from "./common.js";

export async function deployClient({
  options,
  serverServiceName,
  clientServiceName,
}: DeploymentInfo<DeployOptions>): Promise<void> {
  waspSays("Deploying your client now...");

  await buildClient({
    serverServiceName,
    options,
  });

  const webAppArtefactsDir = getWebAppArtefactsDir(options.waspProjectDir);

  const status = await deployServiceWithStreamingLogs(
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
  waspSays(messages[status]);
}

async function buildClient({
  serverServiceName,
  options,
}: {
  serverServiceName: ServerServiceName;
  options: DeployOptions;
}): Promise<void> {
  waspSays("Building web client for production...");
  waspSays(
    `If you configured a custom domain for the server, you should run the command with an env variable: ${serverUrlEnvVarName}=https://serverUrl.com wasp deploy railway deploy`,
  );

  const clientBuildDir = getClientBuildDir(options.waspProjectDir);
  const npmCli = createCommandWithDirectory("npm", clientBuildDir);

  await npmCli(["install"]);

  const serverUrl = getServerUrlFromEnv(
    await generateServiceUrl(serverServiceName, serverAppPort, options),
  );
  await npmCli(["run", "build"], {
    env: {
      ...process.env,
      [serverUrlEnvVarName]: serverUrl,
    },
  });
}
