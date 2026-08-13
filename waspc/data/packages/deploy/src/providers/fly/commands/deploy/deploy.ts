import { $, cd } from "zx";

import { getFullCommandName } from "../../../../common/commander.js";
import {
  displayWaspRocketImage,
  waspSays,
} from "../../../../common/terminal.js";
import { ensureWaspProjectIsBuilt } from "../../../../common/waspBuild.js";
import { getServerDeploymentDir } from "../../../../common/waspProject.js";
import {
  createDeploymentInstructions,
  DeploymentInstructions,
} from "../../DeploymentInstructions.js";
import { getFlyAppUrl } from "../../flyAppUrl.js";
import { secretExists } from "../../flyCli.js";
import { flySetupCommand } from "../../index.js";
import {
  clientTomlExistsInProject,
  copyLocalServerTomlToProject,
  copyProjectServerTomlLocally,
  getInferredBasenameFromServerToml,
  getTomlFilePaths,
  serverTomlExistsInProject,
} from "../../tomlFile.js";
import { DeployCmdOptions } from "./DeployCmdOptions.js";

export async function deploy(cmdOptions: DeployCmdOptions): Promise<void> {
  waspSays("Deploying your Wasp app to Fly.io!");

  await ensureWaspProjectIsBuilt(cmdOptions);

  const tomlFilePaths = getTomlFilePaths(cmdOptions);

  // NOTE: Below, it would be nice if we could store the app and DB names somewhere.
  // For now we just rely on the suffix naming convention and infer from toml files.
  if (!serverTomlExistsInProject(tomlFilePaths)) {
    waspSays(
      `${
        tomlFilePaths.serverTomlPath
      } missing. Skipping deploy. Perhaps you need to run "${getFullCommandName(
        flySetupCommand,
      )}" first?`,
    );
    return;
  }

  if (cmdOptions.skipServer) {
    waspSays("Skipping deploy due to CLI option.");
    return;
  }

  const deploymentInstructions = createDeploymentInstructions({
    baseName: getInferredBasenameFromServerToml(tomlFilePaths),
    cmdOptions,
    tomlFilePaths,
  });

  await deployApp(deploymentInstructions, cmdOptions);

  if (clientTomlExistsInProject(tomlFilePaths)) {
    sayClientAppIsNoLongerDeployed(deploymentInstructions);
  }
}

async function deployApp(
  deploymentInstructions: DeploymentInstructions<DeployCmdOptions>,
  { buildLocally }: DeployCmdOptions,
) {
  waspSays("Deploying your app now...");

  cd(getServerDeploymentDir(deploymentInstructions.cmdOptions.waspProjectDir));
  copyProjectServerTomlLocally(deploymentInstructions.tomlFilePaths);

  // Make sure we have a DATABASE_URL present. If not, they need to create/attach their DB first.
  const databaseUrlSet = await secretExists("DATABASE_URL");
  if (!databaseUrlSet) {
    throw new Error(
      "Your app does not have a DATABASE_URL secret set. Perhaps you need to create or attach your database?",
    );
  }

  const deployArgs = [buildLocally ? "--local-only" : "--remote-only"];
  await $`flyctl deploy ${deployArgs}`;

  // NOTE: Deploy is not expected to update the toml file, but doing this just in case.
  // However, if it does and we fail to copy it back, we would be in an inconsistent state.
  // TOOD: Consider how to best handle this situation across all operations.
  copyLocalServerTomlToProject(deploymentInstructions.tomlFilePaths);

  displayWaspRocketImage();
  waspSays(
    `Your app has been deployed! It is accessible at: ${getFlyAppUrl(deploymentInstructions.serverFlyAppName)}`,
  );
}

/**
 * Apps used to be deployed as two Fly apps, one serving the pages and one the
 * API. One app now serves both, so projects set up before this still have a
 * client app around, which nothing deploys to anymore.
 */
function sayClientAppIsNoLongerDeployed(
  deploymentInstructions: DeploymentInstructions<DeployCmdOptions>,
): void {
  waspSays(
    `Your app now serves its own pages, so its client app (${deploymentInstructions.clientFlyAppName}) is not deployed to anymore.
Point your users at ${getFlyAppUrl(deploymentInstructions.serverFlyAppName)}, and delete the client app when they are:
  flyctl apps destroy ${deploymentInstructions.clientFlyAppName}
  rm ${deploymentInstructions.tomlFilePaths.clientTomlPath}`,
  );
}
