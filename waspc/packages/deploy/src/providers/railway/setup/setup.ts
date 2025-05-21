import { $, cd } from 'zx';
import crypto from 'crypto';

import { createDeploymentInfo, DeploymentInfo } from '../DeploymentInfo.js';
import { SetupOptions } from './SetupOptions.js';
import { waspSays, cdToServerBuildDir, cdToClientBuildDir } from '../../../helpers.js';
import { serverAppPort, clientAppPort } from '../helpers/ports.js';
import { ensureProjectForCurrentDir } from '../helpers/project/index.js';
import { getServiceUrl } from '../helpers/serviceUrl.js';

export async function setup(baseName: string, options: SetupOptions): Promise<void> {
  waspSays('Setting up your Wasp app with Railway!');

  // Railway CLI links projects to the current directory
  cd(options.waspProjectDir);

  const deploymentInfo = createDeploymentInfo(baseName, options);

  const project = await ensureProjectForCurrentDir(deploymentInfo);

  const buildWasp = async () => {
    if (options.skipBuild) {
      return;
    }

    waspSays('Building your Wasp app...');
    cd(options.waspProjectDir);
    await $`${options.waspExe} build`;
  };

  await buildWasp();

  if (project.doesServiceExist(deploymentInfo.dbName)) {
    waspSays('Postgres service already exists. Skipping database setup.');
  } else {
    await setupDb(deploymentInfo);
  }

  if (project.doesServiceExist(deploymentInfo.clientName)) {
    waspSays('Client service already exists. Skipping client setup.');
  } else {
    await setupClient(deploymentInfo);
  }

  if (project.doesServiceExist(deploymentInfo.serverName)) {
    waspSays('Server service already exists. Skipping server setup.');
  } else {
    await setupServer(deploymentInfo);
  }
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
