import { $ } from "zx";

import { generateRandomString } from "../../../../common/random.js";
import { waspSays } from "../../../../common/terminal.js";
import {
  ensureWaspProjectIsBuilt,
  getClientBuildDir,
  getServerBuildDir,
} from "../../../../common/waspProject.js";
import { createCommandWithCwd } from "../../../../common/zx.js";
import { RailwayProjectName } from "../../brandedTypes.js";
import { createDeploymentInfo, DeploymentInfo } from "../../DeploymentInfo.js";
import { clientAppPort, serverAppPort } from "../../ports.js";
import { ensureRailwayProjectForDirectory } from "../../railwayProject/index.js";
import {
  getRailwayDatabaseUrlReference,
  getRailwayPublicUrlReferenceForSelf,
  getRailwayPublicUrlReferenceForService,
} from "../../railwayService/env.js";
import { generateServiceUrl } from "../../railwayService/url.js";
import { SetupOptions } from "./SetupOptions.js";

export async function setup(
  projectName: RailwayProjectName,
  options: SetupOptions,
): Promise<void> {
  waspSays("Setting up your Wasp app with Railway!");

  const deploymentInfo = createDeploymentInfo(projectName, options);

  const project = await ensureRailwayProjectForDirectory(
    options.waspProjectDir,
    deploymentInfo,
  );

  await ensureWaspProjectIsBuilt(options);

  if (project.doesServiceExist(deploymentInfo.dbServiceName)) {
    waspSays("Postgres service already exists. Skipping database setup.");
  } else {
    await setupDb(deploymentInfo);
  }

  if (project.doesServiceExist(deploymentInfo.clientServiceName)) {
    waspSays("Client service already exists. Skipping client setup.");
  } else {
    await setupClient(deploymentInfo);
  }

  if (project.doesServiceExist(deploymentInfo.serverServiceName)) {
    waspSays("Server service already exists. Skipping server setup.");
  } else {
    await setupServer(deploymentInfo);
  }
}

async function setupDb({
  options,
}: DeploymentInfo<SetupOptions>): Promise<void> {
  waspSays("Setting up database");

  const railwayCli = createCommandWithCwd(
    options.railwayExe,
    options.waspProjectDir,
  );
  await railwayCli(["add", "-d", "postgres"]);
}

async function setupServer({
  options,
  serverServiceName,
  clientServiceName,
  dbServiceName,
}: DeploymentInfo<SetupOptions>): Promise<void> {
  waspSays(`Setting up server app with name ${serverServiceName}`);

  // The client service needs a URL so it can be referenced in the
  // server service env variables.
  await generateServiceUrl(clientServiceName, clientAppPort, options);

  const serverBuildDir = getServerBuildDir(options.waspProjectDir);
  const railwayCli = createCommandWithCwd(options.railwayExe, serverBuildDir);

  const clientUrl = getRailwayPublicUrlReferenceForService(clientServiceName);
  const databaseUrl = getRailwayDatabaseUrlReference(dbServiceName);
  const serverUrl = getRailwayPublicUrlReferenceForSelf();
  const jwtSecret = generateRandomString();
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
  options,
  clientServiceName,
}: DeploymentInfo<SetupOptions>): Promise<void> {
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
