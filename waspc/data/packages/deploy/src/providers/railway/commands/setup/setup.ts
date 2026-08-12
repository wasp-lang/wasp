import { WaspProjectDir } from "../../../../common/brandedTypes.js";
import { generateRandomHexString } from "../../../../common/random.js";
import { waspSays } from "../../../../common/terminal.js";
import { ensureWaspProjectIsBuilt } from "../../../../common/waspBuild.js";
import { getServerDeploymentDir } from "../../../../common/waspProject.js";
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
import { getRailwayEnvVarValueReference } from "../../env.js";
import { serverAppPort } from "../../ports.js";
import {
  initRailwayProject,
  linkRailwayProjectToWaspProjectDir,
} from "../../railwayProject/cli.js";
import {
  getRailwayProjectStatus,
  ProjectStatus,
} from "../../railwayProject/index.js";
import { RailwayProject } from "../../railwayProject/RailwayProject.js";
import { waitForServiceDeploymentSuccess } from "../../railwayService/deployment.js";
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

  if (project.doesServiceExist(deploymentInstructions.serverServiceName)) {
    waspSays("App service already exists. Skipping app setup.");
  } else {
    await setupServer(deploymentInstructions);
  }

  if (project.doesServiceExist(deploymentInstructions.clientServiceName)) {
    waspSays(
      `The "${deploymentInstructions.clientServiceName}" service exists, from back when the client was a service of its own.
Your app now serves its own pages, so nothing deploys to that service anymore. You can remove it from your Railway project.`,
    );
  }

  if (options.clientSecret.length > 0) {
    waspSays(
      `Ignoring --client-secret: your app's client environment variables are part of its pages and assets, so they are set when the app's image is built, not when it runs.`,
    );
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
  existingProjectId?: RailwayProjectId;
  workspace?: string;
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
  dbServiceName,
}: DeploymentInstructions<SetupCmdOptions>): Promise<void> {
  waspSays("Setting up database");

  const railwayCli = createCommandWithCwd(
    options.railwayExe,
    options.waspProjectDir,
  );

  if (options.dbImage) {
    waspSays(`Using custom database image: ${options.dbImage}`);
    // When using a custom database image, Railway doesn't automatically set the default
    // Postgres-related environment variables, so we need to set them ourselves.
    await railwayCli([
      "add",
      ...["--service", dbServiceName],
      ...["--image", options.dbImage],
      ...["--variables", "POSTGRES_DB=railway"],
      ...["--variables", "POSTGRES_USER=postgres"],
      ...[
        "--variables",
        `POSTGRES_PASSWORD=${getRailwayEnvVarValueReference("secret()")}`,
      ],
      ...["--variables", "PORT=5432"],
      ...["--variables", "PGDATA=/var/lib/postgresql/data/pgdata"],
      ...[
        "--variables",
        `DATABASE_URL=postgresql://${getRailwayEnvVarValueReference("POSTGRES_USER")}:${getRailwayEnvVarValueReference("POSTGRES_PASSWORD")}@${getRailwayEnvVarValueReference("RAILWAY_PRIVATE_DOMAIN")}:${getRailwayEnvVarValueReference("PORT")}/${getRailwayEnvVarValueReference("POSTGRES_DB")}`,
      ],
    ]);
  } else {
    // Use the default Railway Postgres template.
    await railwayCli(["add", "-d", "postgres"]);
  }

  // The database service deploys asynchronously and `railway add` doesn't wait
  // for it. The server service references the database's DATABASE_URL env
  // variable, and Railway references to a service that isn't fully set up
  // silently resolve to an empty string and never recover.
  await waitForServiceDeploymentSuccess(dbServiceName, options);
}

async function setupServer({
  cmdOptions: options,
  serverServiceName,
  dbServiceName,
}: DeploymentInstructions<SetupCmdOptions>): Promise<void> {
  waspSays(`Setting up app service with name ${serverServiceName}`);

  const serverDeploymentDir = getServerDeploymentDir(options.waspProjectDir);
  const railwayCli = createCommandWithCwd(
    options.railwayExe,
    serverDeploymentDir,
  );

  // One service serves both the app's pages and its API, so both of the URLs
  // below are its own. If we reference the service URL in its OWN env
  // variables, we don't prefix it with the service name.
  const appUrl = `https://${getRailwayEnvVarValueReference("RAILWAY_PUBLIC_DOMAIN")}`;
  const databaseUrl = getRailwayEnvVarValueReference("DATABASE_URL", {
    serviceName: dbServiceName,
  });
  const jwtSecret = generateRandomHexString();
  await railwayCli(
    [
      "add",
      ["--service", serverServiceName],
      ["--variables", `PORT=${serverAppPort}`],
      ["--variables", `JWT_SECRET=${jwtSecret}`],
      ["--variables", `WASP_SERVER_URL=${appUrl}`],
      ["--variables", `WASP_WEB_CLIENT_URL=${appUrl}`],
      ["--variables", `DATABASE_URL=${databaseUrl}`],
      ...options.serverSecret.map((secret) => ["--variables", secret]),
    ].flat(),
  );

  // The server service needs a URL so it can be referenced in the
  // env variables, we can only generate it after the service is created.
  await generateServiceUrl(serverServiceName, serverAppPort, options);

  waspSays("App setup complete!");
}
