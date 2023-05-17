import { TomlFilePaths } from './helpers/tomlFileHelpers.js';
import { CommonOptions } from './CommonOptions.js';

export type DeploymentInfo<Option extends CommonOptions> = Readonly<{
	baseName: string;
	region?: string;
	options: Option;
	tomlFilePaths: TomlFilePaths;
	clientName: string;
	clientUrl: string;
	serverName: string;
	serverUrl: string;
	dbName: string;
}>;

export function createDeploymentInfo<Option extends CommonOptions>(
	baseName: string,
	region: string | undefined,
	options: Option,
	tomlFilePaths: TomlFilePaths,
): DeploymentInfo<Option> {
	return {
		baseName,
		region,
		options,
		tomlFilePaths,
		clientName: `${baseName}-client`,
		clientUrl: `https://${baseName}-client.fly.dev`,
		serverName: `${baseName}-server`,
		serverUrl: `https://${baseName}-server.fly.dev`,
		dbName: `${baseName}-db`,
	};
}
