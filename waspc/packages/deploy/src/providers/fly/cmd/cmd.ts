import { exit } from 'process';
import { $, cd } from 'zx';
import { CommonOps, getCommonOps } from '../helpers/CommonOps.js';
import { buildDirExists, waspSays } from '../helpers/helpers.js';
import { deleteLocalToml, getTomlFilePaths, localTomlExists } from '../helpers/tomlFileHelpers.js';
import { CmdOptions } from './CmdOptions.js';

// Runs a command by copying down the project toml files, executing it, and copying it back up (just in case).
// If the toml file does not exist, some commands will not run with additional args (e.g. -a <appname>).
export async function cmd(flyctlArgs: string[], options: CmdOptions): Promise<void> {
	if (options.org) {
		flyctlArgs.push('--org', options.org);
	}

	waspSays(`Running ${options.context} command: flyctl ${flyctlArgs.join(' ')}`);

	if (!buildDirExists(options.waspProjectDir)) {
		waspSays('Building your Wasp app...');
		cd(options.waspProjectDir);
		await $`${options.waspExe} build`;
	}

	const tomlFilePaths = getTomlFilePaths(options);
	const commonOps = getCommonOps(options.context, options.waspProjectDir, tomlFilePaths);

	await runFlyctlCommand(commonOps, flyctlArgs);
}

async function runFlyctlCommand(commonOps: CommonOps, flyctlArgs: string[]): Promise<void> {
	commonOps.cdToBuildDir();
	deleteLocalToml();
	if (commonOps.tomlExistsInProject()) {
		commonOps.copyProjectTomlLocally();
	}

	try {
		await $`flyctl ${flyctlArgs}`;
	} catch {
		exit(1);
	}

	if (localTomlExists()) {
		commonOps.copyLocalTomlToProject();
	}
}
