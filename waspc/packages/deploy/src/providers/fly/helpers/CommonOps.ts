import { cdToClientBuildDir, cdToServerBuildDir } from './helpers.js';
import * as tomlHelpers from './tomlFileHelpers.js';

export interface CommonOps {
	waspDir: string;
	paths: tomlHelpers.TomlFilePaths;

	cdToBuildDir: () => void;
	tomlExistsInProject: () => boolean;
	copyLocalTomlToProject: () => void;
	copyProjectTomlLocally: () => void;
}

export class ClientCommonOps implements CommonOps {
	waspDir: string;
	paths: tomlHelpers.TomlFilePaths;

	constructor(waspDir: string, paths: tomlHelpers.TomlFilePaths) {
		this.waspDir = waspDir;
		this.paths = paths;
	}

	cdToBuildDir = () => cdToClientBuildDir(this.waspDir);
	tomlExistsInProject = () => tomlHelpers.clientTomlExistsInProject(this.paths);
	copyLocalTomlToProject = () => tomlHelpers.copyLocalClientTomlToProject(this.paths);
	copyProjectTomlLocally = () => tomlHelpers.copyProjectClientTomlLocally(this.paths);
}

export class ServerCommonOps implements CommonOps {
	waspDir: string;
	paths: tomlHelpers.TomlFilePaths;

	constructor(waspDir: string, paths: tomlHelpers.TomlFilePaths) {
		this.waspDir = waspDir;
		this.paths = paths;
	}

	cdToBuildDir = () => cdToServerBuildDir(this.waspDir);
	tomlExistsInProject = () => tomlHelpers.serverTomlExistsInProject(this.paths);
	copyLocalTomlToProject = () => tomlHelpers.copyLocalServerTomlToProject(this.paths);
	copyProjectTomlLocally = () => tomlHelpers.copyProjectServerTomlLocally(this.paths);
}

export enum ContextOption {
	Client = 'client',
	Server = 'server',
}

export function getCommonOps(context: ContextOption, waspDir: string, paths: tomlHelpers.TomlFilePaths): CommonOps {
	const commonOps: Record<ContextOption, CommonOps> = {
		[ContextOption.Client]: new ClientCommonOps(waspDir, paths),
		[ContextOption.Server]: new ServerCommonOps(waspDir, paths),
	};
	return commonOps[context];
}
