import { $, ProcessOutput, question } from "zx";

import { getFullCommandName } from "../../../../common/commander.js";
import { waspSays } from "../../../../common/terminal.js";
import { createDeploymentInstructions } from "../../DeploymentInstructions.js";
import { flyDeployCommand, flySetupCommand } from "../../index.js";
import {
  getInferredBasenameFromServerToml,
  getTomlFilePaths,
  serverTomlExistsInProject,
} from "../../tomlFile.js";
import { CreateDbCmdOptions } from "./CreateDbCmdOptions.js";

export async function createDb(
  region: string,
  cmdOptions: CreateDbCmdOptions,
): Promise<void> {
  waspSays("Creating your DB on Fly.io!");

  const tomlFilePaths = getTomlFilePaths(cmdOptions);

  if (!serverTomlExistsInProject(tomlFilePaths)) {
    throw new Error(
      `${
        tomlFilePaths.serverTomlPath
      } missing. Skipping DB creation. Perhaps you need to run "${getFullCommandName(
        flySetupCommand,
      )}" first?`,
    );
  }

  const inferredBaseName = getInferredBasenameFromServerToml(tomlFilePaths);
  const deploymentInstructions = createDeploymentInstructions({
    baseName: inferredBaseName,
    region,
    cmdOptions,
    tomlFilePaths,
  });

  // Creates a DB, waits for it to come up, then links it to the app.
  // The attachment process shares the DATABASE_URL secret.
  const createArgs = [
    "--name",
    deploymentInstructions.dbName,
    "--region",
    deploymentInstructions.region,
    "--vm-size",
    cmdOptions.vmSize,
    "--initial-cluster-size",
    cmdOptions.initialClusterSize,
    "--volume-size",
    cmdOptions.volumeSize,
  ];

  if (cmdOptions.dbImage) {
    createArgs.push("--image-ref", cmdOptions.dbImage);
  }

  if (deploymentInstructions.cmdOptions.org) {
    createArgs.push("--org", deploymentInstructions.cmdOptions.org);
  }

  try {
    await $`flyctl postgres create ${createArgs}`;
    await $`flyctl postgres attach ${deploymentInstructions.dbName} -a ${deploymentInstructions.serverFlyAppName}`;
  } catch (error) {
    if (error instanceof ProcessOutput) {
      throw new Error(error.stderr.trim());
    } else {
      throw error;
    }
  }

  await question(
    "Please take note of your database credentials above, as they will not be available in plaintext again. Press any key to continue.",
  );

  waspSays(
    `Don't forget to deploy your app by running "${getFullCommandName(flyDeployCommand)}".`,
  );
}
