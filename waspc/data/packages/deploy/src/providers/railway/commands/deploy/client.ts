import fs from "fs";

import { buildClient } from "../../../../common/clientApp.js";
import { isSsrEnabled } from "../../../../common/ssr.js";
import {
  displayWaspRocketImage,
  waspSays,
} from "../../../../common/terminal.js";
import { getClientBuildDir } from "../../../../common/waspProject.js";
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

  const serverServiceUrl =
    options.customServerUrl ??
    (await generateServiceUrl(serverServiceName, serverAppPort, options));

  const ssrEnabled = isSsrEnabled(options.waspProjectDir);
  const clientBuildArtefactsDir = await buildClient(serverServiceUrl, options);
  const clientBuildDir = getClientBuildDir(options.waspProjectDir);

  if (ssrEnabled) {
    const dockerfileContents = `
      FROM node:22.12.0-alpine3.20
      WORKDIR /app
      ENV NODE_ENV=production
      ENV PORT=${clientAppPort}
      COPY package.json package-lock.json ./
      RUN npm ci --omit=dev
      COPY ./build/ ./build/
      COPY ./build-ssr/ ./build-ssr/
      COPY ./server-ssr.mjs ./server-ssr.mjs
      EXPOSE ${clientAppPort}
      CMD [ "node", "server-ssr.mjs" ]
    `;
    fs.writeFileSync(`${clientBuildDir}/Dockerfile`, dockerfileContents);
  }

  const deploymentStatus = await deployServiceWithStreamingLogs(
    {
      name: clientServiceName,
      dirToDeploy: ssrEnabled ? clientBuildDir : clientBuildArtefactsDir,
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
