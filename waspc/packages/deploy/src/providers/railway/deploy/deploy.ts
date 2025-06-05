import { $, cd, ProcessOutput } from "zx";

import { exit } from "process";
import {
  cdToClientBuildDir,
  cdToServerBuildDir,
  displayWaspRocketImage,
  getServerArtefactsDir,
  getWebAppArtefactsDir,
  makeIdempotent,
  waspSays,
} from "../../../helpers.js";
import { createDeploymentInfo, DeploymentInfo } from "../DeploymentInfo.js";
import { clientAppPort, serverAppPort } from "../helpers/ports.js";
import { getProjectForCurrentDir } from "../helpers/project/cli.js";
import { getServiceUrl } from "../helpers/serviceUrl.js";
import { DeployOptions } from "./DeployOptions.js";

export async function deploy(
  baseName: string,
  options: DeployOptions,
): Promise<void> {
  // Railway CLI links projects to the current directory
  cd(options.waspProjectDir);

  const project = await getProjectForCurrentDir(options.railwayExe);
  if (project === null) {
    waspSays(
      'No Railway project detected. Please run "wasp deploy railway setup" first.',
    );
    exit(1);
  }

  waspSays("Deploying your Wasp app to Railway!");

  const buildWasp = makeIdempotent(async () => {
    if (options.skipBuild) {
      return;
    }

    waspSays("Building your Wasp app...");
    cd(options.waspProjectDir);
    await $`${options.waspExe} build`;
  });

  if (options.skipServer) {
    waspSays("Skipping server deploy due to CLI option.");
  } else {
    const deploymentInfo = createDeploymentInfo(baseName, options);
    await buildWasp();
    await deployServer(deploymentInfo);
  }

  if (options.skipClient) {
    waspSays("Skipping client deploy due to CLI option.");
  } else {
    const deploymentInfo = createDeploymentInfo(baseName, options);
    await buildWasp();
    await deployClient(deploymentInfo);
  }
}

async function deployServer({
  options,
  serverName,
}: DeploymentInfo<DeployOptions>) {
  waspSays("Deploying your server now...");

  cdToServerBuildDir(options.waspProjectDir);

  const serverArtefactsDir = getServerArtefactsDir(options.waspProjectDir);

  const status = await deployServiceWithStreamingLogs(options.railwayExe, {
    name: serverName,
    artefactsDirectory: serverArtefactsDir,
  });

  const messages: Record<ServiceDeploymentStatus, string> = {
    [ServiceDeploymentStatus.SUCCESS]: "Server has been deployed!",
    [ServiceDeploymentStatus.FAILED_STREAMING_LOGS]:
      "Server deployment started, but failed to stream build logs. Please check the Railway dashboard for build logs.",
  };

  waspSays(messages[status]);
}

async function deployClient({
  options,
  serverName,
  clientName,
}: DeploymentInfo<DeployOptions>) {
  waspSays("Deploying your client now...");

  cdToClientBuildDir(options.waspProjectDir);

  waspSays("Building web client for production...");
  waspSays(
    "If you configured a custom domain for the server, you should run the command with an env variable: REACT_APP_API_URL=https://serverUrl.com wasp deploy railway deploy",
  );

  const serverUrl = process.env.REACT_APP_API_URL
    ? process.env.REACT_APP_API_URL
    : await getServiceUrl(options.railwayExe, serverName, serverAppPort);
  await $`npm install`;
  await $`REACT_APP_API_URL=${serverUrl} npm run build`;

  const webAppArtefactsDir = getWebAppArtefactsDir(options.waspProjectDir);

  const status = await deployServiceWithStreamingLogs(options.railwayExe, {
    name: clientName,
    artefactsDirectory: webAppArtefactsDir,
  });

  const clientUrl = await getServiceUrl(
    options.railwayExe,
    clientName,
    clientAppPort,
  );

  displayWaspRocketImage();

  const messages: Record<ServiceDeploymentStatus, string> = {
    [ServiceDeploymentStatus.SUCCESS]: `Client has been deployed! Your Wasp app is accessible at: ${clientUrl}`,
    [ServiceDeploymentStatus.FAILED_STREAMING_LOGS]: `Client deployment started, but failed to stream build logs. Your Wasp app should be accessible at: ${clientUrl}`,
  };
  waspSays(messages[status]);
}

enum ServiceDeploymentStatus {
  SUCCESS = "SUCCESS",
  FAILED_STREAMING_LOGS = "FAILED_STREAMING_LOGS",
}

async function deployServiceWithStreamingLogs(
  railwayExe: string,
  service: {
    name: string;
    artefactsDirectory: string;
  },
): Promise<ServiceDeploymentStatus> {
  try {
    const commandArgs = [
      service.artefactsDirectory,
      "--service",
      service.name,
      "--no-gitignore",
      "--path-as-root",
      "--ci",
    ];
    await $`${railwayExe} up ${commandArgs}`;
    return ServiceDeploymentStatus.SUCCESS;
  } catch (e: unknown) {
    if (isFailedToStreamLogsError(e)) {
      // Continue with the deployment, but notify the user about the known issue.
      waspSays(
        `Failed to stream build log for service "${service.name}". This sometimes happens with the Railway CLI. Please check the Railway dashboard for build logs.`,
      );
      return ServiceDeploymentStatus.FAILED_STREAMING_LOGS;
    }

    waspSays(
      `Error deploying service "${service.name}", please check the output above.`,
    );
    exit(1);
  }
}

function isFailedToStreamLogsError(error: unknown): boolean {
  return (
    isProcessOutputError(error) &&
    error.stderr.includes("Failed to stream build log")
  );
}

function isProcessOutputError(error: unknown): error is ProcessOutput {
  return error instanceof ProcessOutput;
}
