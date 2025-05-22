import { $, cd } from 'zx';

import { DeployOptions } from './DeployOptions.js';
import { createDeploymentInfo, DeploymentInfo } from '../DeploymentInfo.js';
import {
  waspSays,
  makeIdempotent,
  cdToServerBuildDir,
  cdToClientBuildDir,
  displayWaspRocketImage,
  getWebAppArtefactsDir,
  getServerArtefactsDir,
} from '../../../helpers.js';
import { clientAppPort, serverAppPort } from '../helpers/ports.js';
import { exit } from 'process';
import { getProjectForCurrentDir } from '../helpers/project/cli.js';
import { getServiceUrl } from '../helpers/serviceUrl.js';

export async function deploy(baseName: string, options: DeployOptions): Promise<void> {
  // Railway CLI links projects to the current directory
  cd(options.waspProjectDir);

  const project = await getProjectForCurrentDir(options.railwayExe);
  if (project === null) {
    waspSays('No Railway project detected. Please run "wasp deploy railway setup" first.');
    exit(1);
  }

  waspSays('Deploying your Wasp app to Railway!');

  const buildWasp = makeIdempotent(async () => {
    if (options.skipBuild) {
      return;
    }

    waspSays('Building your Wasp app...');
    cd(options.waspProjectDir);
    await $`${options.waspExe} build`;
  });

  if (options.skipServer) {
    waspSays('Skipping server deploy due to CLI option.');
  } else {
    const deploymentInfo = createDeploymentInfo(baseName, options);
    await buildWasp();
    await deployServer(deploymentInfo);
  }

  if (options.skipClient) {
    waspSays('Skipping client deploy due to CLI option.');
  } else {
    const deploymentInfo = createDeploymentInfo(baseName, options);
    await buildWasp();
    await deployClient(deploymentInfo);
  }
}

async function deployServer({ options, serverName }: DeploymentInfo<DeployOptions>) {
  waspSays('Deploying your server now...');

  cdToServerBuildDir(options.waspProjectDir);

  const serverArtefactsDir = getServerArtefactsDir(options.waspProjectDir);

  await $`${options.railwayExe} up ${serverArtefactsDir} --service "${serverName}" --no-gitignore --path-as-root --ci`;

  waspSays('Server has been deployed!');
}

async function deployClient({ options, serverName, clientName }: DeploymentInfo<DeployOptions>) {
  waspSays('Deploying your client now...');

  cdToClientBuildDir(options.waspProjectDir);

  waspSays('Building web client for production...');
  waspSays(
    'If you configured a custom domain for the server, you should run the command with an env variable: REACT_APP_API_URL=https://serverUrl.com wasp deploy railway deploy',
  );

  const serverUrl = process.env.REACT_APP_API_URL
    ? process.env.REACT_APP_API_URL
    : await getServiceUrl(options.railwayExe, serverName, serverAppPort);
  await $`npm install`;
  await $`REACT_APP_API_URL=${serverUrl} npm run build`;

  const webAppArtefactsDir = getWebAppArtefactsDir(options.waspProjectDir);
  await $`${options.railwayExe} up ${webAppArtefactsDir} --service "${clientName}" --no-gitignore --path-as-root --ci`;

  const clientUrl = await getServiceUrl(options.railwayExe, clientName, clientAppPort);

  displayWaspRocketImage();
  waspSays(`Client has been deployed! Your Wasp app is accessible at: ${clientUrl}`);
}
