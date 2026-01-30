import { $, cd, fs } from "zx";

import { buildClient } from "../../../../common/clientApp.js";
import { getFullCommandName } from "../../../../common/commander.js";
import {
  displayWaspRocketImage,
  waspSays,
} from "../../../../common/terminal.js";
import { ensureWaspProjectIsBuilt } from "../../../../common/waspBuild.js";
import {
  getClientBuildDir,
  getServerBuildDir,
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

  cd(getServerBuildDir(deploymentInstructions.cmdOptions.waspProjectDir));
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

  cd(getClientBuildDir(deploymentInstructions.cmdOptions.waspProjectDir));
  copyProjectClientTomlLocally(deploymentInstructions.tomlFilePaths);

  const serverFlyAppUrl = getFlyAppUrl(deploymentInstructions.serverFlyAppName);

  await buildClient(serverFlyAppUrl, deploymentInstructions.cmdOptions);

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

  copyLocalClientTomlToProject(deploymentInstructions.tomlFilePaths);

  displayWaspRocketImage();
  waspSays(
    `Client has been deployed! Your Wasp app is accessible at: ${getFlyAppUrl(deploymentInstructions.clientFlyAppName)}`,
  );
}
