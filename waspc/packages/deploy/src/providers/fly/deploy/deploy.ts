import { exit } from "process";
import { $, fs } from "zx";
import { makeIdempotentWaspBuildCommand } from "../../../common/build.js";
import {
  getServerAppUrlFromEnv,
  serverUrlEnvVarName,
} from "../../../common/clientApp.js";
import { getCommandName } from "../../../common/commander.js";
import { displayWaspRocketImage, waspSays } from "../../../common/terminal.js";
import {
  cdToClientBuildDir,
  cdToServerBuildDir,
} from "../../../common/waspProject.js";
import { createDeploymentInfo, DeploymentInfo } from "../DeploymentInfo.js";
import { secretExists } from "../helpers/flyctlHelpers.js";
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
} from "../helpers/tomlFileHelpers.js";
import { flySetupCommand } from "../index.js";
import { DeployOptions } from "./DeployOptions.js";

export async function deploy(options: DeployOptions): Promise<void> {
  waspSays("Deploying your Wasp app to Fly.io!");

  const buildWasp = makeIdempotentWaspBuildCommand(options);

  const tomlFilePaths = getTomlFilePaths(options);

  // NOTE: Below, it would be nice if we could store the client, server, and DB names somewhere.
  // For now we just rely on the suffix naming convention and infer from toml files.
  if (!serverTomlExistsInProject(tomlFilePaths)) {
    waspSays(
      `${
        tomlFilePaths.serverTomlPath
      } missing. Skipping server deploy. Perhaps you need to run "${getCommandName(
        flySetupCommand,
      )}" first?`,
    );
  } else if (options.skipServer) {
    waspSays("Skipping server deploy due to CLI option.");
  } else {
    const inferredBaseName = getInferredBasenameFromServerToml(tomlFilePaths);
    const deploymentInfo = createDeploymentInfo(
      inferredBaseName,
      undefined,
      options,
      tomlFilePaths,
    );
    await buildWasp();
    await deployServer(deploymentInfo, options);
  }

  if (!clientTomlExistsInProject(tomlFilePaths)) {
    waspSays(
      `${
        tomlFilePaths.clientTomlPath
      } missing. Skipping client deploy. Perhaps you need to run "${getCommandName(
        flySetupCommand,
      )}" first?`,
    );
  } else if (options.skipClient) {
    waspSays("Skipping client deploy due to CLI option.");
  } else {
    const inferredBaseName = getInferredBasenameFromClientToml(tomlFilePaths);
    const deploymentInfo = createDeploymentInfo(
      inferredBaseName,
      undefined,
      options,
      tomlFilePaths,
    );
    await buildWasp();
    await deployClient(deploymentInfo, options);
  }
}

async function deployServer(
  deploymentInfo: DeploymentInfo<DeployOptions>,
  { buildLocally }: DeployOptions,
) {
  waspSays("Deploying your server now...");

  cdToServerBuildDir(deploymentInfo.options.waspProjectDir);
  copyProjectServerTomlLocally(deploymentInfo.tomlFilePaths);

  // Make sure we have a DATABASE_URL present. If not, they need to create/attach their DB first.
  try {
    const databaseUrlSet = await secretExists("DATABASE_URL");
    if (!databaseUrlSet) {
      waspSays(
        "Your server app does not have a DATABASE_URL secret set. Perhaps you need to create or attach your database?",
      );
      exit(1);
    }
  } catch (e) {
    waspSays("Unable to check for DATABASE_URL secret.");
    exit(1);
  }

  const deployArgs = [buildLocally ? "--local-only" : "--remote-only"];
  await $`flyctl deploy ${deployArgs}`;

  // NOTE: Deploy is not expected to update the toml file, but doing this just in case.
  // However, if it does and we fail to copy it back, we would be in an inconsistent state.
  // TOOD: Consider how to best handle this situation across all operations.
  copyLocalServerTomlToProject(deploymentInfo.tomlFilePaths);

  waspSays("Server has been deployed!");
}

async function deployClient(
  deploymentInfo: DeploymentInfo<DeployOptions>,
  { buildLocally }: DeployOptions,
) {
  waspSays("Deploying your client now...");

  cdToClientBuildDir(deploymentInfo.options.waspProjectDir);
  copyProjectClientTomlLocally(deploymentInfo.tomlFilePaths);

  waspSays("Building web client for production...");
  waspSays(
    `If you configured a custom domain for the server, you should run the command with an env variable: ${serverUrlEnvVarName}=https://serverUrl.com wasp deploy fly deploy`,
  );

  const serverUrl = getServerAppUrlFromEnv(deploymentInfo.serverUrl);
  await $`npm install`;
  await $`${serverUrlEnvVarName}=${serverUrl} npm run build`;

  // Creates the necessary Dockerfile for deploying static websites to Fly.io.
  // Adds dummy .dockerignore to supress CLI question.
  // Ref: https://fly.io/docs/languages-and-frameworks/static/
  const dockerfileContents = `
		FROM pierrezemb/gostatic
		CMD [ "-fallback", "index.html" ]
		COPY ./build/ /srv/http/
	`;
  fs.writeFileSync("Dockerfile", dockerfileContents);
  fs.writeFileSync(".dockerignore", "");

  const deployArgs = [buildLocally ? "--local-only" : "--remote-only"];
  await $`flyctl deploy ${deployArgs}`;

  copyLocalClientTomlToProject(deploymentInfo.tomlFilePaths);

  displayWaspRocketImage();
  waspSays(
    `Client has been deployed! Your Wasp app is accessible at: ${deploymentInfo.clientUrl}`,
  );
}
