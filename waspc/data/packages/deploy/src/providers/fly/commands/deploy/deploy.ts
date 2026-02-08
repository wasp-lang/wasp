import { $, cd, fs } from "zx";

import {
  buildClient,
  getSsrServerFileName,
  isSsrEnabled,
} from "../../../../common/clientApp.js";
import { getFullCommandName } from "../../../../common/commander.js";
import {
  displayWaspRocketImage,
  waspSays,
} from "../../../../common/terminal.js";
import { ensureWaspProjectIsBuilt } from "../../../../common/waspBuild.js";
import {
  getClientDeploymentDir,
  getServerDeploymentDir,
} from "../../../../common/waspProject.js";
import {
  createDeploymentInstructions,
  DeploymentInstructions,
} from "../../DeploymentInstructions.js";
import { getFlyAppUrl } from "../../flyAppUrl.js";
import { secretExists } from "../../flyCli.js";
import { flySetupCommand } from "../../index.js";
import {
  clientTomlExistsInProject,
  copyLocalClientTomlToProject,
  copyLocalServerTomlToProject,
  copyProjectClientTomlLocally,
  copyProjectServerTomlLocally,
  getInferredBasenameFromClientToml,
  getInferredBasenameFromServerToml,
  getTomlFilePaths,
  serverTomlExistsInProject,
} from "../../tomlFile.js";
import { DeployCmdOptions } from "./DeployCmdOptions.js";

// Port for SSR Node.js server (must match what the SSR server listens on via PORT env var)
const ssrServerPort = 3000;

export async function deploy(cmdOptions: DeployCmdOptions): Promise<void> {
  waspSays("Deploying your Wasp app to Fly.io!");

  await ensureWaspProjectIsBuilt(cmdOptions);

  const tomlFilePaths = getTomlFilePaths(cmdOptions);

  // NOTE: Below, it would be nice if we could store the client, server, and DB names somewhere.
  // For now we just rely on the suffix naming convention and infer from toml files.
  if (!serverTomlExistsInProject(tomlFilePaths)) {
    waspSays(
      `${
        tomlFilePaths.serverTomlPath
      } missing. Skipping server deploy. Perhaps you need to run "${getFullCommandName(
        flySetupCommand,
      )}" first?`,
    );
  } else if (cmdOptions.skipServer) {
    waspSays("Skipping server deploy due to CLI option.");
  } else {
    const inferredBaseName = getInferredBasenameFromServerToml(tomlFilePaths);
    const deploymentInstructions = createDeploymentInstructions({
      baseName: inferredBaseName,
      cmdOptions,
      tomlFilePaths,
    });
    await deployServer(deploymentInstructions, cmdOptions);
  }

  if (!clientTomlExistsInProject(tomlFilePaths)) {
    waspSays(
      `${
        tomlFilePaths.clientTomlPath
      } missing. Skipping client deploy. Perhaps you need to run "${getFullCommandName(
        flySetupCommand,
      )}" first?`,
    );
  } else if (cmdOptions.skipClient) {
    waspSays("Skipping client deploy due to CLI option.");
  } else {
    const inferredBaseName = getInferredBasenameFromClientToml(tomlFilePaths);
    const deploymentInstructions = createDeploymentInstructions({
      baseName: inferredBaseName,
      cmdOptions,
      tomlFilePaths,
    });
    await deployClient(deploymentInstructions, cmdOptions);
  }
}

async function deployServer(
  deploymentInstructions: DeploymentInstructions<DeployCmdOptions>,
  { buildLocally }: DeployCmdOptions,
) {
  waspSays("Deploying your server now...");

  cd(getServerDeploymentDir(deploymentInstructions.cmdOptions.waspProjectDir));
  copyProjectServerTomlLocally(deploymentInstructions.tomlFilePaths);

  // Make sure we have a DATABASE_URL present. If not, they need to create/attach their DB first.
  const databaseUrlSet = await secretExists("DATABASE_URL");
  if (!databaseUrlSet) {
    throw new Error(
      "Your server app does not have a DATABASE_URL secret set. Perhaps you need to create or attach your database?",
    );
  }

  const deployArgs = [buildLocally ? "--local-only" : "--remote-only"];
  await $`flyctl deploy ${deployArgs}`;

  // NOTE: Deploy is not expected to update the toml file, but doing this just in case.
  // However, if it does and we fail to copy it back, we would be in an inconsistent state.
  // TOOD: Consider how to best handle this situation across all operations.
  copyLocalServerTomlToProject(deploymentInstructions.tomlFilePaths);

  waspSays("Server has been deployed!");
}

async function deployClient(
  deploymentInstructions: DeploymentInstructions<DeployCmdOptions>,
  { buildLocally }: DeployCmdOptions,
) {
  waspSays("Deploying your client now...");

  const waspProjectDir = deploymentInstructions.cmdOptions.waspProjectDir;
  cd(getClientDeploymentDir(waspProjectDir));
  copyProjectClientTomlLocally(deploymentInstructions.tomlFilePaths);

  const serverUrl =
    deploymentInstructions.cmdOptions.customServerUrl ??
    getFlyAppUrl(deploymentInstructions.serverFlyAppName);

  await buildClient(serverUrl, deploymentInstructions.cmdOptions);

  // Check if SSR is enabled to determine deployment strategy
  const ssrEnabled = isSsrEnabled(waspProjectDir);

  // Creates the necessary Dockerfile for deploying to Fly.io.
  // Adds dummy .dockerignore to suppress CLI question.
  let dockerfileContents: string;

  if (ssrEnabled) {
    // SSR deployment: Use Node.js to run the SSR server
    // Ref: https://fly.io/docs/languages-and-frameworks/node/
    waspSays("SSR is enabled. Deploying as Node.js server...");
    const ssrServerFile = getSsrServerFileName();
    dockerfileContents = `
FROM node:22-alpine3.20
RUN apk --no-cache -U upgrade
WORKDIR /app
# Install dependencies first (cached layer unless package.json changes).
# --ignore-scripts prevents @prisma/client postinstall from running before
# schema.prisma is available in the build context.
COPY package.json .
RUN npm install --production --ignore-scripts
# Copy the built application (includes schema.prisma, build/, build-ssr/)
COPY . .
# Generate the Prisma client from the schema bundled with the web-app.
# The Vite SSR build externalizes @prisma/client (native query engine),
# so it must be available as a real node_module at runtime.
RUN npx prisma generate --schema=./schema.prisma
ENV PORT=${ssrServerPort}
EXPOSE ${ssrServerPort}
CMD ["node", "${ssrServerFile}"]
`;
  } else {
    // Static deployment: Use goStatic to serve static files
    // Ref: https://fly.io/docs/languages-and-frameworks/static/
    dockerfileContents = `
FROM pierrezemb/gostatic
CMD [ "-fallback", "index.html" ]
COPY ./build/ /srv/http/
`;
  }

  fs.writeFileSync("Dockerfile", dockerfileContents);
  fs.writeFileSync(".dockerignore", "");

  const deployArgs = [buildLocally ? "--local-only" : "--remote-only"];
  await $`flyctl deploy ${deployArgs}`;

  copyLocalClientTomlToProject(deploymentInstructions.tomlFilePaths);

  displayWaspRocketImage();
  waspSays(
    `Client has been deployed! Your Wasp app is accessible at: ${getFlyAppUrl(deploymentInstructions.clientFlyAppName)}`,
  );
}
