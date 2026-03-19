import { WaspProjectDir } from "../../../../common/brandedTypes.js";
import { getFullCommandName } from "../../../../common/commander.js";
import { waspSays } from "../../../../common/terminal.js";
import { ensureWaspProjectIsBuilt } from "../../../../common/waspBuild.js";
import {
  RailwayCliExe,
  RailwayProjectId,
  RailwayProjectName,
} from "../../brandedTypes.js";
import { createDeploymentInstructions } from "../../DeploymentInstructions.js";
import { railwaySetupCommand } from "../../index.js";
import { linkRailwayProjectToWaspProjectDir } from "../../railwayProject/cli.js";
import { getRailwayProjectStatus } from "../../railwayProject/index.js";
import { LinkedRailwayProject } from "../../railwayProject/RailwayProject.js";

import { deployClient } from "./client.js";
import { DeployCmdOptions } from "./DeployCmdOptions.js";
import { deployServer } from "./server.js";

export async function deploy(
  projectName: RailwayProjectName,
  options: DeployCmdOptions,
): Promise<void> {
  const deploymentInstructions = createDeploymentInstructions(
    projectName,
    options,
  );

  await ensureRailwayProjectForDirectory({
    projectName,
    waspProjectDir: options.waspProjectDir,
    existingProjectId: options.existingProjectId,
    railwayExe: options.railwayExe,
  });

  waspSays("Deploying your Wasp app to Railway!");

  await ensureWaspProjectIsBuilt(options);

  if (options.skipServer) {
    waspSays("Skipping server deploy due to CLI option.");
  } else {
    await deployServer(deploymentInstructions);
  }

  if (options.skipClient) {
    waspSays("Skipping client deploy due to CLI option.");
  } else {
    await deployClient(deploymentInstructions);
  }
}

async function ensureRailwayProjectForDirectory({
  railwayExe,
  projectName,
  waspProjectDir,
  existingProjectId,
}: {
  railwayExe: RailwayCliExe;
  projectName: RailwayProjectName;
  waspProjectDir: WaspProjectDir;
  existingProjectId?: RailwayProjectId;
}): Promise<LinkedRailwayProject> {
  const railwayProject = await getRailwayProjectStatus({
    projectName,
    waspProjectDir,
    railwayExe,
    existingProjectId,
  });

  switch (railwayProject.status) {
    case "linked":
      waspSays(`Using already linked project: "${projectName}"`);
      return railwayProject;

    case "unlinked":
      waspSays(
        `Linking Railway project with name "${railwayProject.name}" to project directory.`,
      );
      return linkRailwayProjectToWaspProjectDir(railwayProject, {
        railwayExe,
        waspProjectDir,
      });

    case "draft":
      throw new Error(
        `No Railway project found. Run ${getFullCommandName(railwaySetupCommand)} first or provide an existing project ID with "--existing-project-id" option.`,
      );

    default:
      railwayProject satisfies never;
      throw new Error(
        `Unhandled project status: ${(railwayProject as { status: string }).status}`,
      );
  }
}
