import { TomlFilePaths } from './helpers/tomlFileHelpers.js';
import { GlobalOptions } from './GlobalOptions.js';

export type DeploymentInfo = Readonly<{
	baseName: string;
	region?: string;
	options: GlobalOptions;
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
	options: GlobalOptions,
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
