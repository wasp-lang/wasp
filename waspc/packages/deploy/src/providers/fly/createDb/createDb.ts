import { exit } from "process";
import { $, question } from "zx";
import { createDeploymentInfo } from "../DeploymentInfo.js";
import { getCommandHelp, waspSays } from "../helpers/helpers.js";
import {
  getInferredBasenameFromServerToml,
  getTomlFilePaths,
  serverTomlExistsInProject,
} from "../helpers/tomlFileHelpers.js";
import { flyDeployCommand, flySetupCommand } from "../index.js";
import { CreateDbOptions } from "./CreateDbOptions.js";

export async function createDb(
  region: string,
  options: CreateDbOptions,
): Promise<void> {
  waspSays("Creating your DB on Fly.io!");

  const tomlFilePaths = getTomlFilePaths(options);

  if (!serverTomlExistsInProject(tomlFilePaths)) {
    waspSays(
      `${
        tomlFilePaths.serverTomlPath
      } missing. Skipping DB creation. Perhaps you need to run "${getCommandHelp(
        flySetupCommand,
      )}" first?`,
    );
    exit(1);
  }

  const inferredBaseName = getInferredBasenameFromServerToml(tomlFilePaths);
  const deploymentInfo = createDeploymentInfo(
    inferredBaseName,
    region,
    options,
    tomlFilePaths,
  );

  // Creates a DB, waits for it to come up, then links it to the app.
  // The attachment process shares the DATABASE_URL secret.
  const createArgs = [
    "--name",
    deploymentInfo.dbName,
    "--region",
    deploymentInfo.region,
    "--vm-size",
    options.vmSize,
    "--initial-cluster-size",
    options.initialClusterSize,
    "--volume-size",
    options.volumeSize,
  ];

  if (deploymentInfo.options.org) {
    createArgs.push("--org", deploymentInfo.options.org);
  }

  await $`flyctl postgres create ${createArgs}`;
  await $`flyctl postgres attach ${deploymentInfo.dbName} -a ${deploymentInfo.serverName}`;

  await question(
    "Please take note of your database credentials above, as they will not be available in plaintext again. Press any key to continue.",
  );

  waspSays(
    `Don't forget to deploy your app by running "${getCommandHelp(flyDeployCommand)}".`,
  );
}
