import { waspSays } from "../../../../common/terminal.js";
import { ensureWaspProjectIsBuilt } from "../../../../common/waspProject.js";
import { RailwayProjectName } from "../../brandedTypes.js";
import { createDeploymentInfo } from "../../DeploymentInfo.js";
import { getRailwayProjectForDirectory } from "../../project/cli.js";

import { deployClient } from "./client.js";
import { DeployOptions } from "./DeployOptions.js";
import { deployServer } from "./server.js";

export async function deploy(
  projectName: RailwayProjectName,
  options: DeployOptions,
): Promise<void> {
  const project = await getRailwayProjectForDirectory(
    options.railwayExe,
    options.waspProjectDir,
  );
  if (project === null) {
    throw new Error(
      'No Railway project detected. Please run "wasp deploy railway setup" first.',
    );
  }

  waspSays("Deploying your Wasp app to Railway!");

  const deploymentInfo = createDeploymentInfo(projectName, options);

  await ensureWaspProjectIsBuilt(options);

  if (options.skipServer) {
    waspSays("Skipping server deploy due to CLI option.");
  } else {
    await deployServer(deploymentInfo);
  }

  if (options.skipClient) {
    waspSays("Skipping client deploy due to CLI option.");
  } else {
    await deployClient(deploymentInfo);
  }
}
