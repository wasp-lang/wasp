import { $ } from "zx";

import { makeIdempotentWaspBuildCommand } from "../../../common/build.js";
import { createCommandWithDirectory } from "../../../common/cli.js";
import { generateRandomJwtSecret } from "../../../common/jwt.js";
import { waspSays } from "../../../common/output.js";
import {
  getClientBuildDir,
  getServerBuildDir,
} from "../../../common/waspProject.js";
import {
  ClientServiceName,
  createDeploymentInfo,
  DeploymentInfo,
  RailwayProjectName,
  ServerServiceName,
} from "../DeploymentInfo.js";
import { clientAppPort, serverAppPort } from "../helpers/ports.js";
import { ensureProjectForDirectory } from "../helpers/project/index.js";
import { generateServiceUrl } from "../helpers/serviceUrl.js";
import { SetupOptions } from "./SetupOptions.js";

export async function setup(
  projectName: RailwayProjectName,
  options: SetupOptions,
): Promise<void> {
  waspSays("Setting up your Wasp app with Railway!");

  const deploymentInfo = createDeploymentInfo(projectName, options);

  const project = await ensureProjectForDirectory(
    options.waspProjectDir,
    deploymentInfo,
  );

  const buildWasp = makeIdempotentWaspBuildCommand(options);

  await buildWasp();

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

  const railwayCli = createCommandWithDirectory(
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
  const railwayCli = createCommandWithDirectory(
    options.railwayExe,
    serverBuildDir,
  );

  const clientUrl = getRailwayPublicUrlReference(clientServiceName);
  const serverUrl = getRailwayPublicUrlReference();
  const jwtSecret = generateRandomJwtSecret();
  const addCmdArgs = [
    ["--service", serverServiceName],
    ["--variables", `PORT=${serverAppPort}`],
    ["--variables", `JWT_SECRET=${jwtSecret}`],
    ["--variables", `WASP_SERVER_URL=${serverUrl}`],
    ["--variables", `WASP_WEB_CLIENT_URL=${clientUrl}`],
    ["--variables", `DATABASE_URL=\${{${dbServiceName}.DATABASE_URL}}`],
    ...options.serverSecret.map((secret) => ["--variables", secret]),
  ].flat();
  await railwayCli(["add", ...addCmdArgs]);

  // The server service needs a URL so it can be referenced in the
  // env variables.
  await generateServiceUrl(serverServiceName, serverAppPort, options);

  waspSays("Server setup complete!");
}

// Uses the special Railway env variable "RAILWAY_PUBLIC_DOMAIN"
// to reference the public domain of a service.
function getRailwayPublicUrlReference(
  serviceName?: ClientServiceName | ServerServiceName,
): string {
  const publicDomainEnvVarName = serviceName
    ? `${serviceName}.RAILWAY_PUBLIC_DOMAIN`
    : "RAILWAY_PUBLIC_DOMAIN";

  return "https://${{" + publicDomainEnvVarName + "}}";
}

async function setupClient({
  options,
  clientServiceName,
}: DeploymentInfo<SetupOptions>): Promise<void> {
  waspSays(`Setting up client app with name ${clientServiceName}`);

  const clientBuildDir = getClientBuildDir(options.waspProjectDir);
  const railwayCli = createCommandWithDirectory(
    options.railwayExe,
    clientBuildDir,
  );

  // Having a Staticfile tells Railway to use a static file server.
  await $({ cwd: clientBuildDir })`touch Staticfile`;

  const addCmdArgs = [
    ["--service", clientServiceName],
    ["--variables", `PORT=${clientAppPort}`],
    ...options.clientSecret.map((secret) => ["--variables", secret]),
  ].flat();
  await railwayCli(["add", ...addCmdArgs]);

  waspSays("Client setup complete!");
}
