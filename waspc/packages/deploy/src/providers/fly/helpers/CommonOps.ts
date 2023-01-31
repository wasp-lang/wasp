import { cdToClientBuildDir, cdToServerBuildDir } from './helpers.js';
import {
	clientTomlExistsInProject,
	copyLocalClientTomlToProject,
	copyLocalServerTomlToProject,
	copyProjectClientTomlLocally,
	copyProjectServerTomlLocally,
	serverTomlExistsInProject,
	TomlFilePaths,
} from './tomlFileHelpers.js';

export type CommonOps = Readonly<{
	waspProjectDir: string;
	paths: TomlFilePaths;
	cdToBuildDir: () => void;
	tomlExistsInProject: () => boolean;
	copyLocalTomlToProject: () => void;
	copyProjectTomlLocally: () => void;
}>;

export function createClientCommonOps(
	waspProjectDir: string,
	paths: TomlFilePaths,
): CommonOps {
	return {
		waspProjectDir,
		paths,
		cdToBuildDir: () => cdToClientBuildDir(waspProjectDir),
		tomlExistsInProject: () => clientTomlExistsInProject(paths),
		copyLocalTomlToProject: () => copyLocalClientTomlToProject(paths),
		copyProjectTomlLocally: () => copyProjectClientTomlLocally(paths),
	};
}

export function createServerCommonOps(
	waspProjectDir: string,
	paths: TomlFilePaths,
): CommonOps {
	return {
		waspProjectDir,
		paths,
		cdToBuildDir: () => cdToServerBuildDir(waspProjectDir),
		tomlExistsInProject: () => serverTomlExistsInProject(paths),
		copyLocalTomlToProject: () => copyLocalServerTomlToProject(paths),
		copyProjectTomlLocally: () => copyProjectServerTomlLocally(paths),
	};
}

export enum ContextOption {
	Client = 'client',
	Server = 'server',
}

export function getCommonOps(context: ContextOption, waspProjectDir: string, paths: TomlFilePaths): CommonOps {
	const commonOps: Record<ContextOption, CommonOps> = {
		[ContextOption.Client]: createClientCommonOps(waspProjectDir, paths),
		[ContextOption.Server]: createServerCommonOps(waspProjectDir, paths),
	};
	return commonOps[context];
}
