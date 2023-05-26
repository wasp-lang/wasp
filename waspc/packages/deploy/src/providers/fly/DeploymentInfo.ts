import { TomlFilePaths } from './helpers/tomlFileHelpers.js';
import { CommonOptions } from './CommonOptions.js';

export type DeploymentInfo<CommandOptions extends CommonOptions> = Readonly<{
	baseName: string;
	region?: string;
	options: CommandOptions;
	tomlFilePaths: TomlFilePaths;
	clientName: string;
	clientUrl: string;
	serverName: string;
	serverUrl: string;
	dbName: string;
}>;

export function createDeploymentInfo<CommandOptions extends CommonOptions>(
	baseName: string,
	region: string | undefined,
	options: CommandOptions,
	tomlFilePaths: TomlFilePaths,
): DeploymentInfo<CommandOptions> {
	return Object.freeze({
		baseName,
		region,
		options,
		tomlFilePaths,
		clientName: `${baseName}-client`,
		clientUrl: `https://${baseName}-client.fly.dev`,
		serverName: `${baseName}-server`,
		serverUrl: `https://${baseName}-server.fly.dev`,
		dbName: `${baseName}-db`,
	});
}
