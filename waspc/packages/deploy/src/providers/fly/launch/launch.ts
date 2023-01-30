import { exit } from 'process';
import { CreateDbOptions } from '../createDb/CreateDbOptions';
import { DeployOptions } from '../deploy/DeployOptions.js';
import { getCommandHelp, waspSays } from '../helpers/helpers.js';
import { setup } from '../setup/setup.js';
import { createDb } from '../createDb/createDb.js';
import { deploy } from '../deploy/deploy.js';
import * as tomlHelpers from '../helpers/tomlFileHelpers.js';
import { createFlyDbCommand, flyDeployCommand, flySetupCommand } from '../index.js';

export async function launch(basename: string, region: string, options: CreateDbOptions): Promise<void> {
	waspSays('Launching your Wasp app to Fly.io!');

	const tomlFiles = tomlHelpers.getTomlFilePaths(options);
	if (tomlHelpers.serverTomlExistsInProject(tomlFiles) || tomlHelpers.clientTomlExistsInProject(tomlFiles)) {
		waspSays('You already have Fly toml files. The launch command is intended to be run one time on a new Fly project. Please try a different command.');
		exit(1);
	}

	try {
		await setup(basename, region, options);
	} catch (e) {
		console.error(e);
		waspSays(`There was an error running "${getCommandHelp(flySetupCommand)}". Please review the error and try again (if appropriate).`);
		exit(1);
	}

	try {
		await createDb(region, options);
	} catch (e) {
		console.error(e);
		waspSays(`There was an error running "${getCommandHelp(createFlyDbCommand)}". Please review the error and try again (if appropriate).`);
		exit(1);
	}

	try {
		const deployOptions: DeployOptions = { ...options, skipBuild: true };
		await deploy(deployOptions);
	} catch (e) {
		console.error(e);
		waspSays(`There was an error running "${getCommandHelp(flyDeployCommand)}". Please review the error and try again (if appropriate).`);
		exit(1);
	}
}
