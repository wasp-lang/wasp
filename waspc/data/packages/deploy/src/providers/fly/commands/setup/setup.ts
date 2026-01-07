import { $, cd, chalk, question } from "zx";

import { getFullCommandName } from "../../../../common/commander.js";
import { generateRandomHexString } from "../../../../common/random.js";
import { waspSays } from "../../../../common/terminal.js";
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
import { createFlyDbCommand } from "../../index.js";
import {
  clientTomlExistsInProject,
  copyLocalClientTomlToProject,
  copyLocalServerTomlToProject,
  deleteLocalToml,
  doesLocalTomlContainLine,
  getTomlFilePaths,
  replaceLineInLocalToml,
  serverTomlExistsInProject,
} from "../../tomlFile.js";
import { SetupCmdOptions } from "./SetupCmdOptions.js";

const internalPortOptionRegex = /internal_port = \d+/g;
const serverAppPort = 8080;
const clientAppPort = 8043;

export async function setup(
  baseName: string,
  region: string,
  cmdOptions: SetupCmdOptions,
): Promise<void> {
  waspSays("Setting up your Wasp app with Fly.io!");

  await ensureWaspProjectIsBuilt(cmdOptions);

  const tomlFilePaths = getTomlFilePaths(cmdOptions);
  const deploymentInstructions = createDeploymentInstructions({
    baseName,
    region,
    cmdOptions,
    tomlFilePaths,
  });

  if (serverTomlExistsInProject(tomlFilePaths)) {
    waspSays(`${tomlFilePaths.serverTomlPath} exists. Skipping server setup.`);
  } else {
    await setupServer(deploymentInstructions);
  }

  if (clientTomlExistsInProject(tomlFilePaths)) {
    waspSays(`${tomlFilePaths.clientTomlPath} exists. Skipping client setup.`);
  } else {
    await setupClient(deploymentInstructions);
  }

  waspSays(
    `Don't forget to create your database by running "${getFullCommandName(createFlyDbCommand)}".`,
  );
}

async function setupServer(
  deploymentInstructions: DeploymentInstructions<SetupCmdOptions>,
) {
  waspSays(
    `Setting up server app with name ${deploymentInstructions.serverFlyAppName}`,
  );

  cd(getServerBuildDir(deploymentInstructions.cmdOptions.waspProjectDir));
  deleteLocalToml();

  const launchArgs = [
    "--name",
    deploymentInstructions.serverFlyAppName,
    "--region",
    deploymentInstructions.region,
  ];

  if (deploymentInstructions.cmdOptions.org) {
    launchArgs.push("--org", deploymentInstructions.cmdOptions.org);
  }

  // This creates the fly.toml file, but does not attempt to deploy.
  await $`flyctl launch --no-deploy ${launchArgs}`;

  const minMachinesOptionRegex = /min_machines_running = 0/g;

  if (!doesLocalTomlContainLine(minMachinesOptionRegex)) {
    await question(`\n⚠️  There was a possible issue setting up your server app.
We tried modifying your server fly.toml to set ${chalk.bold(
      "min_machines_running = 1",
    )}, but couldn't find the option ${chalk.bold("min_machines_running")} in the fly.toml.

We advise that you additionaly check what is the value for "minimal number of machines running" on Fly
for this server app and confirm that it is set to the value you are OK with.

Be aware that if it is set to 0, your server will shut down when there are no requests from the client,
which might be an issue for you if you have recurring Jobs or some other processes that need to keep
running on the server even without external input, in which case we advise keeping "minimal number
of machines running" setting at a number larger than zero.

Contact the Wasp Team at our Discord server if you need help with this: https://discord.gg/rzdnErX

Press any key to continue or Ctrl+C to cancel.`);
  } else {
    replaceLineInLocalToml(minMachinesOptionRegex, "min_machines_running = 1");
  }

  if (!doesLocalTomlContainLine(internalPortOptionRegex)) {
    await question(`\n⚠️  There was an issue setting up your server app.
We tried modifying your server fly.toml to set ${chalk.bold(
      `internal_port = ${serverAppPort}`,
    )}, but couldn't find the option ${chalk.bold("internal_port")} in the fly.toml.

This means your server app might not be accessible.

Contact the Wasp Team at our Discord server if you need help with this: https://discord.gg/rzdnErX

Press any key to continue or Ctrl+C to cancel.`);
  } else {
    replaceLineInLocalToml(
      internalPortOptionRegex,
      `internal_port = ${serverAppPort}`,
    );
  }

  copyLocalServerTomlToProject(deploymentInstructions.tomlFilePaths);

  const jwtSecret = generateRandomHexString();

  const secretsArgs = [
    `JWT_SECRET=${jwtSecret}`,
    // NOTE: Normally these would just be envars, but flyctl
    // doesn't provide a way to set envars that persist to fly.toml.
    `PORT=${serverAppPort}`,
    `WASP_WEB_CLIENT_URL=${getFlyAppUrl(deploymentInstructions.clientFlyAppName)}`,
    `WASP_SERVER_URL=${getFlyAppUrl(deploymentInstructions.serverFlyAppName)}`,
  ];

  if (deploymentInstructions.cmdOptions.serverSecret.length > 0) {
    deploymentInstructions.cmdOptions.serverSecret.forEach((secret) => {
      secretsArgs.push(secret);
    });
  }

  await $`flyctl secrets set ${secretsArgs}`;

  console.log(""); // `flyctl secrets` does not produce it's own newline.
  waspSays("Server setup complete!");
}

async function setupClient(
  deploymentInstructions: DeploymentInstructions<SetupCmdOptions>,
) {
  waspSays(
    `Setting up client app with name ${deploymentInstructions.clientFlyAppName}`,
  );

  cd(getClientBuildDir(deploymentInstructions.cmdOptions.waspProjectDir));
  deleteLocalToml();

  const launchArgs = [
    "--name",
    deploymentInstructions.clientFlyAppName,
    "--region",
    deploymentInstructions.region,
  ];

  if (deploymentInstructions.cmdOptions.org) {
    launchArgs.push("--org", deploymentInstructions.cmdOptions.org);
  }

  // This creates the fly.toml file, but does not attempt to deploy.
  await $`flyctl launch --no-deploy ${launchArgs}`;

  if (!doesLocalTomlContainLine(internalPortOptionRegex)) {
    await question(`\n⚠️  There was an issue setting up your client app.
We tried modifying your client fly.toml to set ${chalk.bold(
      `internal_port = ${clientAppPort}`,
    )}, but couldn't find the option ${chalk.bold("internal_port")} in the fly.toml.

This means your client app might not be accessible.

Contact the Wasp Team at our Discord server if you need help with this: https://discord.gg/rzdnErX

Press any key to continue or Ctrl+C to cancel.`);
  } else {
    // goStatic listens on port 8043 by default, but the default fly.toml
    // assumes port 8080 (or 3000, depending on flyctl version).
    replaceLineInLocalToml(
      internalPortOptionRegex,
      `internal_port = ${clientAppPort}`,
    );
  }

  copyLocalClientTomlToProject(deploymentInstructions.tomlFilePaths);

  if (deploymentInstructions.cmdOptions.clientSecret.length > 0) {
    await $`flyctl secrets set ${deploymentInstructions.cmdOptions.clientSecret}`;
  }

  waspSays("Client setup complete!");
}
