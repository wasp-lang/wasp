import { $, question } from 'zx';
import { exit } from 'process';
import * as tomlHelpers from '../helpers/tomlFileHelpers.js';
import { createDeploymentInfo } from '../DeploymentInfo.js';
import { CreateDbOptions } from './CreateDbOptions.js';
import { getInferredBasenameFromServerToml } from '../helpers/tomlFileHelpers.js';
import { getCommandHelp, waspSays } from '../helpers/helpers.js';
import { flyDeployCommand, flySetupCommand } from '../index.js';

export async function createDb(region: string, options: CreateDbOptions): Promise<void> {
	waspSays('Creating your DB on Fly.io!');

	const tomlFiles = tomlHelpers.getTomlFilePaths(options);

	if (!tomlHelpers.serverTomlExistsInProject(tomlFiles)) {
		waspSays(`${tomlFiles.serverTomlPath} missing. Skipping DB creation. Perhaps you need to run "${getCommandHelp(flySetupCommand)}" first?`);
		exit(1);
	}

	const inferredBaseName = getInferredBasenameFromServerToml(tomlFiles);
	const deploymentInfo = createDeploymentInfo(inferredBaseName, region, options, tomlFiles);

	// Creates a DB, waits for it to come up, then links it to the app.
	// The attachment process shares the DATABASE_URL secret.
	const createArgs = [
		'--name', deploymentInfo.dbName,
		'--region', deploymentInfo.region,
		'--vm-size', options.vmSize,
		'--initial-cluster-size', options.initialClusterSize,
		'--volume-size', options.volumeSize,
	];
	await $`flyctl postgres create ${createArgs}`;
	await $`flyctl postgres attach ${deploymentInfo.dbName} -a ${deploymentInfo.serverName}`;

	await question('Please take note of your database credentials above, as they will not be available in plaintext again. Press any key to continue.');

	waspSays(`Don't forget to deploy your app by running "${getCommandHelp(flyDeployCommand)}".`);
}
