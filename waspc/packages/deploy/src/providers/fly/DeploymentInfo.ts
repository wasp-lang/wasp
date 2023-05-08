import { TomlFilePaths } from './helpers/tomlFileHelpers.js';
import { CommonOptions } from './CommonOptions.js';

export type DeploymentInfo<OptionType extends CommonOptions> = Readonly<{
	baseName: string;
	region?: string;
	options: OptionType;
	tomlFilePaths: TomlFilePaths;
	clientName: string;
	clientUrl: string;
	serverName: string;
	serverUrl: string;
	dbName: string;
}>;

export function createDeploymentInfo<OptionType extends CommonOptions>(
	baseName: string,
	region: string | undefined,
	options: OptionType,
	tomlFilePaths: TomlFilePaths,
): DeploymentInfo<OptionType> {
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
