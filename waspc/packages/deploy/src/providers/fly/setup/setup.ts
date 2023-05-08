import { $, cd } from 'zx';
import crypto from 'crypto';
import {
	clientTomlExistsInProject,
	copyLocalClientTomlToProject,
	copyLocalServerTomlToProject,
	deleteLocalToml,
	getTomlFilePaths,
	replaceLineInLocalToml,
	serverTomlExistsInProject,
} from '../helpers/tomlFileHelpers.js';
import { createDeploymentInfo, DeploymentInfo } from '../DeploymentInfo.js';
import { SetupOptions } from './SetupOptions.js';
import { cdToClientBuildDir, cdToServerBuildDir, makeIdempotent, getCommandHelp, waspSays } from '../helpers/helpers.js';
import { createFlyDbCommand } from '../index.js';

export async function setup(baseName: string, region: string, options: SetupOptions): Promise<void> {
	waspSays('Setting up your Wasp app with Fly.io!');

	const buildWasp = makeIdempotent(async () => {
		waspSays('Building your Wasp app...');
		cd(options.waspProjectDir);
		await $`${options.waspExe} build`;
	});

	const tomlFilePaths = getTomlFilePaths(options);
	const deploymentInfo = createDeploymentInfo(baseName, region, options, tomlFilePaths);

	if (serverTomlExistsInProject(tomlFilePaths)) {
		waspSays(`${tomlFilePaths.serverTomlPath} exists. Skipping server setup.`);
	} else {
		await buildWasp();
		await setupServer(deploymentInfo);
	}

	if (clientTomlExistsInProject(tomlFilePaths)) {
		waspSays(`${tomlFilePaths.clientTomlPath} exists. Skipping client setup.`);
	} else {
		await buildWasp();
		await setupClient(deploymentInfo);
	}

	waspSays(`Don't forget to create your database by running "${getCommandHelp(createFlyDbCommand)}".`);
}

async function setupServer(deploymentInfo: DeploymentInfo<SetupOptions>) {
	waspSays(`Setting up server app with name ${deploymentInfo.serverName}`);

	cdToServerBuildDir(deploymentInfo.options.waspProjectDir);
	deleteLocalToml();

	const launchArgs = [
		'--name', deploymentInfo.serverName,
		'--region', deploymentInfo.region,
	];

	if (deploymentInfo.options.org) {
		launchArgs.push('--org', deploymentInfo.options.org);
	}

	// This creates the fly.toml file, but does not attempt to deploy.
	await $`flyctl launch --no-deploy ${launchArgs}`;

	copyLocalServerTomlToProject(deploymentInfo.tomlFilePaths);

	const randomString = crypto.randomBytes(32).toString('hex');

	const secretsArgs = [
		`JWT_SECRET=${randomString}`,
		// NOTE: Normally these would just be envars, but flyctl
		// doesn't provide a way to set envars that persist to fly.toml.
		'PORT=8080',
		`WASP_WEB_CLIENT_URL=${deploymentInfo.clientUrl}`,
	];

	if (deploymentInfo.options.serverSecret) {
		deploymentInfo.options.serverSecret.forEach(secret => {
			secretsArgs.push(secret);
		});
	}

	await $`flyctl secrets set ${secretsArgs}`;

	console.log(''); // `flyctl secrets` does not produce it's own newline.
	waspSays('Server setup complete!');
}

async function setupClient(deploymentInfo: DeploymentInfo<SetupOptions>) {
	waspSays(`Setting up client app with name ${deploymentInfo.clientName}`);

	cdToClientBuildDir(deploymentInfo.options.waspProjectDir);
	deleteLocalToml();

	const launchArgs = [
		'--name', deploymentInfo.clientName,
		'--region', deploymentInfo.region,
	];

	if (deploymentInfo.options.org) {
		launchArgs.push('--org', deploymentInfo.options.org);
	}

	// This creates the fly.toml file, but does not attempt to deploy.
	await $`flyctl launch --no-deploy  ${launchArgs}`;

	// goStatic listens on port 8043 by default, but the default fly.toml assumes port 8080.
	replaceLineInLocalToml(/internal_port = 8080/g, 'internal_port = 8043');

	copyLocalClientTomlToProject(deploymentInfo.tomlFilePaths);

	if (deploymentInfo.options.clientSecret) {
		await $`flyctl secrets set ${deploymentInfo.options.clientSecret}`;
	}

	waspSays('Client setup complete!');
}
