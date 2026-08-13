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
import {
  getRailwayProjectStatus,
  ProjectStatus,
} from "../../railwayProject/index.js";
import { RailwayProject } from "../../railwayProject/RailwayProject.js";

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
    waspSays("Skipping deploy due to CLI option.");
    return;
  }

  await deployServer(deploymentInstructions);

  // Apps used to be deployed as two services, one serving the pages and one the
  // API. One service now serves both, so projects set up before this still have
  // a client service around, which nothing deploys to anymore.
  waspSays(
    `Your app now serves its own pages, so the "${deploymentInstructions.clientServiceName}" service is not deployed to anymore.
Point your users at your app's own URL, and remove that service from your Railway project when they are.`,
  );
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
}): Promise<RailwayProject> {
  const { status, project } = await getRailwayProjectStatus({
    projectName,
    waspProjectDir,
    railwayExe,
    existingProjectId,
  });

  switch (status) {
    case ProjectStatus.EXISTING_PROJECT_ALREADY_LINKED:
      waspSays(`Using already linked project: "${projectName}"`);
      return project;

    case ProjectStatus.EXISTING_PROJECT_SHOULD_BE_LINKED:
      waspSays(
        `Linking Railway project with name "${project.name}" to project directory.`,
      );
      return linkRailwayProjectToWaspProjectDir(project, {
        railwayExe,
        waspProjectDir,
      });

    case ProjectStatus.MISSING_PROJECT:
      throw new Error(
        `No Railway project found. Run ${getFullCommandName(railwaySetupCommand)} first or provide an existing project ID with "--existing-project-id" option.`,
      );

    default:
      status satisfies never;
      throw new Error(`Unhandled status: ${status}`);
  }
}
