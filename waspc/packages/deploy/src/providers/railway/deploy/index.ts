import { exit } from "process";
import { makeIdempotentWaspBuildCommand } from "../../../common/build.js";
import { waspSays } from "../../../common/terminal.js";
import { createDeploymentInfo, RailwayProjectName } from "../DeploymentInfo.js";
import { getProjectForDirectory } from "../helpers/project/cli.js";
import { deployClient } from "./client.js";
import { DeployOptions } from "./DeployOptions.js";
import { deployServer } from "./server.js";

export async function deploy(
  projectName: RailwayProjectName,
  options: DeployOptions,
): Promise<void> {
  const project = await getProjectForDirectory(
    options.railwayExe,
    options.waspProjectDir,
  );
  if (project === null) {
    waspSays(
      'No Railway project detected. Please run "wasp deploy railway setup" first.',
    );
    exit(1);
  }

  waspSays("Deploying your Wasp app to Railway!");

  const deploymentInfo = createDeploymentInfo(projectName, options);

  const buildWasp = makeIdempotentWaspBuildCommand(options);

  if (options.skipServer) {
    waspSays("Skipping server deploy due to CLI option.");
  } else {
    await buildWasp();
    await deployServer(deploymentInfo);
  }

  if (options.skipClient) {
    waspSays("Skipping client deploy due to CLI option.");
  } else {
    await buildWasp();
    await deployClient(deploymentInfo);
  }
}
