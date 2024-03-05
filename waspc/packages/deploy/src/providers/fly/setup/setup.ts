import { $, cd, question } from 'zx';
import crypto from 'crypto';
import {
	clientTomlExistsInProject,
	copyLocalClientTomlToProject,
	copyLocalServerTomlToProject,
	deleteLocalToml,
	doesLocalTomlContainLine,
	getTomlFilePaths,
	replaceLineInLocalToml,
	serverTomlExistsInProject,
} from '../helpers/tomlFileHelpers.js';
import { createDeploymentInfo, DeploymentInfo } from '../DeploymentInfo.js';
import { SetupOptions } from './SetupOptions.js';
import {
	cdToClientBuildDir,
	cdToServerBuildDir,
	makeIdempotent,
	getCommandHelp,
	waspSays,
	boldText,
} from '../helpers/helpers.js';
import { createFlyDbCommand } from '../index.js';

export async function setup(
	baseName: string,
	region: string,
	options: SetupOptions,
): Promise<void> {
	waspSays('Setting up your Wasp app with Fly.io!');

	const buildWasp = makeIdempotent(async () => {
		waspSays('Building your Wasp app...');
		cd(options.waspProjectDir);
		await $`${options.waspExe} build`;
	});

	const tomlFilePaths = getTomlFilePaths(options);
	const deploymentInfo = createDeploymentInfo(
		baseName,
		region,
		options,
		tomlFilePaths,
	);

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

	waspSays(
		`Don't forget to create your database by running "${getCommandHelp(
			createFlyDbCommand,
		)}".`,
	);
}

async function setupServer(deploymentInfo: DeploymentInfo<SetupOptions>) {
	waspSays(`Setting up server app with name ${deploymentInfo.serverName}`);

	cdToServerBuildDir(deploymentInfo.options.waspProjectDir);
	deleteLocalToml();

	const launchArgs = [
		'--name',
		deploymentInfo.serverName,
		'--region',
		deploymentInfo.region,
	];

	if (deploymentInfo.options.org) {
		launchArgs.push('--org', deploymentInfo.options.org);
	}

	// This creates the fly.toml file, but does not attempt to deploy.
	await $`flyctl launch --no-deploy ${launchArgs}`;

	const minMachinesOptionRegex = /min_machines_running = 0/g;

	if (!doesLocalTomlContainLine(minMachinesOptionRegex)) {
		await question(`\n⚠️  There was a possible issue setting up your server app.
We tried modifying your server fly.toml to set ${boldText(
		'min_machines_running = 1',
	)}, but couldn't find the option ${boldText(
	'min_machines_running',
)} in the fly.toml.

We advise that you additionaly check what is the value for "minimal number of machines running" on Fly
for this server app and confirm that it is set to the value you are OK with.

Be aware that if it is set to 0, your server will shut down when there are no requests from the client,
which might be an issue for you if you have recurring Jobs or some other processes that need to keep
running on the server even without external input, in which case we advise keeping "minimal number
of machines running" setting at a number larger than zero.

Contact the Wasp Team at our Discord server if you need help with this: https://discord.gg/rzdnErX 

Press any key to continue or Ctrl+C to cancel.`);
	} else {
		replaceLineInLocalToml(minMachinesOptionRegex, 'min_machines_running = 1');
	}

	copyLocalServerTomlToProject(deploymentInfo.tomlFilePaths);

	const randomString = crypto.randomBytes(32).toString('hex');

	const secretsArgs = [
		`JWT_SECRET=${randomString}`,
		// NOTE: Normally these would just be envars, but flyctl
		// doesn't provide a way to set envars that persist to fly.toml.
		'PORT=8080',
		`WASP_WEB_CLIENT_URL=${deploymentInfo.clientUrl}`,
		`WASP_SERVER_URL=${deploymentInfo.serverUrl}`,
	];

	if (deploymentInfo.options.serverSecret.length > 0) {
		deploymentInfo.options.serverSecret.forEach((secret) => {
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
		'--name',
		deploymentInfo.clientName,
		'--region',
		deploymentInfo.region,
	];

	if (deploymentInfo.options.org) {
		launchArgs.push('--org', deploymentInfo.options.org);
	}

	// This creates the fly.toml file, but does not attempt to deploy.
	await $`flyctl launch --no-deploy  ${launchArgs}`;

	const internalPortOptionRegex = /internal_port = \d+/g;

	if (!doesLocalTomlContainLine(internalPortOptionRegex)) {
		await question(`\n⚠️  There was an issue setting up your client app.
We tried modifying your client fly.toml to set ${boldText(
		'internal_port = 8043',
	)}, but couldn't find the option ${boldText(
	'internal_port',
)} in the fly.toml.

This means your client app might not be accessible.

Contact the Wasp Team at our Discord server if you need help with this: https://discord.gg/rzdnErX 

Press any key to continue or Ctrl+C to cancel.`);
	} else {
		// goStatic listens on port 8043 by default, but the default fly.toml
		// assumes port 8080 (or 3000, depending on flyctl version).
		replaceLineInLocalToml(internalPortOptionRegex, 'internal_port = 8043');
	}

	copyLocalClientTomlToProject(deploymentInfo.tomlFilePaths);

	if (deploymentInfo.options.clientSecret.length > 0) {
		await $`flyctl secrets set ${deploymentInfo.options.clientSecret}`;
	}

	waspSays('Client setup complete!');
}
