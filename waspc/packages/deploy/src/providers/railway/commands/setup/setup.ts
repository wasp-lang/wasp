import { $ } from "zx";

import { WaspProjectDir } from "../../../../common/brandedTypes.js";
import { generateRandomHexString } from "../../../../common/random.js";
import { waspSays } from "../../../../common/terminal.js";
import { ensureWaspProjectIsBuilt } from "../../../../common/waspBuild.js";
import {
  getClientBuildDir,
  getServerBuildDir,
} from "../../../../common/waspProject.js";
import { createCommandWithCwd } from "../../../../common/zx.js";
import {
  RailwayCliExe,
  RailwayProjectId,
  RailwayProjectName,
} from "../../brandedTypes.js";
import {
  createDeploymentInstructions,
  DeploymentInstructions,
} from "../../DeploymentInstructions.js";
import { clientAppPort, serverAppPort } from "../../ports.js";
import {
  initRailwayProject,
  linkRailwayProjectToWaspProjectDir,
} from "../../railwayProject/cli.js";
import {
  getRailwayProjectStatus,
  ProjectStatus,
} from "../../railwayProject/index.js";
import { RailwayProject } from "../../railwayProject/RailwayProject.js";
import { generateServiceUrl } from "../../railwayService/url.js";
import { SetupCmdOptions } from "./SetupCmdOptions.js";

export async function setup(
  projectName: RailwayProjectName,
  options: SetupCmdOptions,
): Promise<void> {
  waspSays("Setting up your Wasp app with Railway!");

  const deploymentInstructions = createDeploymentInstructions(
    projectName,
    options,
  );

  const project = await setupRailwayProjectForDirectory({
    projectName,
    existingProjectId: options.existingProjectId,
    waspProjectDir: options.waspProjectDir,
    railwayExe: options.railwayExe,
    workspace: options.workspace,
  });

  await ensureWaspProjectIsBuilt(options);

  if (project.doesServiceExist(deploymentInstructions.dbServiceName)) {
    waspSays("Postgres service already exists. Skipping database setup.");
  } else {
    await setupDb(deploymentInstructions);
  }

  if (project.doesServiceExist(deploymentInstructions.clientServiceName)) {
    waspSays("Client service already exists. Skipping client setup.");
  } else {
    await setupClient(deploymentInstructions);
  }

  if (project.doesServiceExist(deploymentInstructions.serverServiceName)) {
    waspSays("Server service already exists. Skipping server setup.");
  } else {
    await setupServer(deploymentInstructions);
  }
}

async function setupRailwayProjectForDirectory({
  railwayExe,
  projectName,
  waspProjectDir,
  existingProjectId,
  workspace,
}: {
  railwayExe: RailwayCliExe;
  projectName: RailwayProjectName;
  waspProjectDir: WaspProjectDir;
  existingProjectId: RailwayProjectId | null;
  workspace: string | null;
}): Promise<RailwayProject> {
  const { status, project } = await getRailwayProjectStatus({
    projectName,
    waspProjectDir,
    railwayExe,
    existingProjectId,
  });

  switch (status) {
    case ProjectStatus.EXISTING_PROJECT_ALREADY_LINKED:
      waspSays(
        `Project with name "${projectName}" already linked. Skipping project creation.`,
      );
      return project;

    case ProjectStatus.EXISTING_PROJECT_SHOULD_BE_LINKED:
      waspSays(`Linking project with name "${project.name}" to this directory`);
      return linkRailwayProjectToWaspProjectDir(project, {
        railwayExe,
        waspProjectDir,
      });

    case ProjectStatus.MISSING_PROJECT:
      waspSays(`Setting up Railway project with name "${projectName}"`);
      return initRailwayProject({
        projectName,
        railwayExe,
        waspProjectDir,
        workspace,
      });

    default:
      status satisfies never;
      throw new Error(`Unhandled status: ${status}`);
  }
}

async function setupDb({
  cmdOptions: options,
}: DeploymentInstructions<SetupCmdOptions>): Promise<void> {
  waspSays("Setting up database");

  const railwayCli = createCommandWithCwd(
    options.railwayExe,
    options.waspProjectDir,
  );
  await railwayCli(["add", "-d", "postgres"]);
}

async function setupServer({
  cmdOptions: options,
  serverServiceName,
  clientServiceName,
  dbServiceName,
}: DeploymentInstructions<SetupCmdOptions>): Promise<void> {
  waspSays(`Setting up server app with name ${serverServiceName}`);

  // The client service needs a URL so it can be referenced in the
  // server service env variables.
  await generateServiceUrl(clientServiceName, clientAppPort, options);

  const serverBuildDir = getServerBuildDir(options.waspProjectDir);
  const railwayCli = createCommandWithCwd(options.railwayExe, serverBuildDir);

  const clientUrl = `https://${getRailwayEnvVarValueReference(`${clientServiceName}.RAILWAY_PUBLIC_DOMAIN`)}`;
  // If we reference the service URL in its OWN env variables, we don't prefix it with the service name.
  const serverUrl = `https://${getRailwayEnvVarValueReference("RAILWAY_PUBLIC_DOMAIN")}`;
  const databaseUrl = getRailwayEnvVarValueReference(
    `${dbServiceName}.DATABASE_URL`,
  );
  const jwtSecret = generateRandomHexString();
  await railwayCli(
    [
      "add",
      ["--service", serverServiceName],
      ["--variables", `PORT=${serverAppPort}`],
      ["--variables", `JWT_SECRET=${jwtSecret}`],
      ["--variables", `WASP_SERVER_URL=${serverUrl}`],
      ["--variables", `WASP_WEB_CLIENT_URL=${clientUrl}`],
      ["--variables", `DATABASE_URL=${databaseUrl}`],
      ...options.serverSecret.map((secret) => ["--variables", secret]),
    ].flat(),
  );

  // The server service needs a URL so it can be referenced in the
  // env variables, we can only generate it after the service is created.
  await generateServiceUrl(serverServiceName, serverAppPort, options);

  waspSays("Server setup complete!");
}

async function setupClient({
  cmdOptions: options,
  clientServiceName,
}: DeploymentInstructions<SetupCmdOptions>): Promise<void> {
  waspSays(`Setting up client app with name ${clientServiceName}`);

  const clientBuildDir = getClientBuildDir(options.waspProjectDir);
  const railwayCli = createCommandWithCwd(options.railwayExe, clientBuildDir);

  // Having a Staticfile tells Railway to use a static file server.
  await $({ cwd: clientBuildDir })`touch Staticfile`;

  await railwayCli(
    [
      "add",
      ["--service", clientServiceName],
      ["--variables", `PORT=${clientAppPort}`],
      ...options.clientSecret.map((secret) => ["--variables", secret]),
    ].flat(),
  );

  waspSays("Client setup complete!");
}

function getRailwayEnvVarValueReference(name: string): string {
  return "${{" + name + "}}";
}
