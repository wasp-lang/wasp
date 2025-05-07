import { $, cd } from 'zx';
import crypto from 'crypto';
import { exit } from 'process';

import { createDeploymentInfo, DeploymentInfo } from '../DeploymentInfo.js';
import { SetupOptions } from './SetupOptions.js';
import {
  waspSays,
  makeIdempotent,
  cdToServerBuildDir,
  cdToClientBuildDir,
} from '../../../helpers.js';
import { serverAppPort, clientAppPort } from '../helpers/ports.js';
import { getExistingProject, getServiceUrl } from '../helpers/railwayHelpers.js';

export async function setup(baseName: string, options: SetupOptions): Promise<void> {
  waspSays('Setting up your Wasp app with Railway!');

  // Railway CLI links projects to the current directory
  cd(options.waspProjectDir);

  const deploymentInfo = createDeploymentInfo(baseName, options);

  // TODO: existing project in this dir, but what if we are in the CI? Then this check won't work.
  // We should provide a way for user to say: "use this existing project" and we run "railway link"
  // instead of "railway init".
  const existingProject = await getExistingProject(options.railwayExe);

  // If the existing project name is different from the base name, we can't proceed.
  if (existingProject && existingProject.projectName !== baseName) {
    waspSays(
      `Project with a different name already linked to this directory: ${existingProject.projectName}. Run "railway unlink" to unlink it.`,
    );
    exit(1);
  } else if (existingProject) {
    waspSays(`Project with name ${baseName} already exists. Skipping project creation.`);
  } else {
    await initProject(deploymentInfo);
    // Check if the project was created successfully...
    const newlyCreatedProject = await getExistingProject(options.railwayExe);
    if (!newlyCreatedProject) {
      waspSays('Project creation failed. Exiting...');
      exit(1);
    } else {
      waspSays('Project created successfully!');
    }
  }

  const buildWasp = makeIdempotent(async () => {
    if (options.skipBuild) {
      return;
    }

    waspSays('Building your Wasp app...');
    cd(options.waspProjectDir);
    await $`${options.waspExe} build`;
  });

  await buildWasp();

  if (existingProject && existingProject.serviceNames.includes(deploymentInfo.dbName)) {
    waspSays('Postgres service already exists. Skipping database setup.');
  } else {
    await setupDb(deploymentInfo);
  }

  if (existingProject && existingProject.serviceNames.includes(deploymentInfo.clientName)) {
    waspSays('Client service already exists. Skipping client setup.');
  } else {
    await setupClient(deploymentInfo);
  }

  if (existingProject && existingProject.serviceNames.includes(deploymentInfo.serverName)) {
    waspSays('Server service already exists. Skipping server setup.');
  } else {
    await setupServer(deploymentInfo);
  }
}

async function initProject({ baseName, options }: DeploymentInfo<SetupOptions>): Promise<void> {
  waspSays(`Setting up Railway project with name ${baseName}`);

  await $({
    // If there are multiple workspaces, the user needs to select **interactively**
    // which one to use. We need to allow users to select the workspace interactively.
    // There is no way to pass it as a command line argument.
    stdio: 'inherit',
  })`${options.railwayExe} init --name ${baseName}`;
}

async function setupDb({ options }: DeploymentInfo<SetupOptions>): Promise<void> {
  waspSays('Setting up database');

  await $`${options.railwayExe} add -d postgres`;
}

async function setupServer({
  options,
  serverName,
  clientName,
  dbName,
}: DeploymentInfo<SetupOptions>): Promise<void> {
  waspSays(`Setting up server app with name ${serverName}`);

  cdToServerBuildDir(options.waspProjectDir);

  // Making sure the client URL is available before setting up the server
  // to have the ${{clientName.RAILWAY_PUBLIC_DOMAIN} variable available.
  await getServiceUrl(options.railwayExe, clientName, clientAppPort);

  const clientUrl = `https://\${{${clientName}.RAILWAY_PUBLIC_DOMAIN}}`;
  const serverUrl = 'https://${{RAILWAY_PUBLIC_DOMAIN}}';
  const jwtSecret = crypto.randomBytes(32).toString('hex');
  const addCmdArgs = [
    'add',
    ['--service', serverName],
    ['--variables', `PORT=${serverAppPort}`],
    ['--variables', `JWT_SECRET=${jwtSecret}`],
    ['--variables', `WASP_SERVER_URL=${serverUrl}`],
    ['--variables', `WASP_WEB_CLIENT_URL=${clientUrl}`],
    ['--variables', `DATABASE_URL=\${{${dbName}.DATABASE_URL}}`],
    ...options.serverSecret.map((secret) => ['--variables', secret]),
  ].flat();

  await $`${options.railwayExe} ${addCmdArgs}`;

  // Making sure the server URL is available before deploying the server
  // to have the ${{RAILWAY_PUBLIC_DOMAIN}} variable available.
  await getServiceUrl(options.railwayExe, serverName, clientAppPort);

  waspSays('Server setup complete!');
}

async function setupClient({ options, clientName }: DeploymentInfo<SetupOptions>): Promise<void> {
  waspSays(`Setting up client app with name ${clientName}`);

  cdToClientBuildDir(options.waspProjectDir);

  // Having a Staticfile tells Railway to use a static file server.
  await $`touch Staticfile`;

  const addCmdArgs = [
    'add',
    ['--service', clientName],
    ['--variables', `PORT=${clientAppPort}`],
    ...options.clientSecret.map((secret) => ['--variables', secret]),
  ].flat();

  await $`${options.railwayExe} ${addCmdArgs}`;

  waspSays('Client setup complete!');
}
