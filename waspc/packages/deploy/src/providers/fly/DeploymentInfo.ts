import { TomlFilePaths } from './helpers/tomlFileHelpers.js';
import { CommonOptions } from './CommonOptions.js';

export type DeploymentInfo = Readonly<{
	baseName: string;
	region?: string;
	options: CommonOptions;
	tomlFilePaths: TomlFilePaths;
	clientName: string;
	clientUrl: string;
	serverName: string;
	serverUrl: string;
	dbName: string;
}>;

export function createDeploymentInfo(
	baseName: string,
	region: string | undefined,
	options: CommonOptions,
	tomlFilePaths: TomlFilePaths,
): DeploymentInfo {
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
