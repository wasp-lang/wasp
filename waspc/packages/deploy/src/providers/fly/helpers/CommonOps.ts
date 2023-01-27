import { cdToClientBuildDir, cdToServerBuildDir } from './helpers.js';
import * as tomlHelpers from './tomlFileHelpers.js';

export type CommonOps = Readonly<{
	waspDir: string;
	paths: tomlHelpers.TomlFilePaths;
	cdToBuildDir: () => void;
	tomlExistsInProject: () => boolean;
	copyLocalTomlToProject: () => void;
	copyProjectTomlLocally: () => void;
}>;

export function createClientCommonOps(
	waspDir: string,
	paths: tomlHelpers.TomlFilePaths,
): CommonOps {
	return {
		waspDir,
		paths,
		cdToBuildDir: () => cdToClientBuildDir(waspDir),
		tomlExistsInProject: () => tomlHelpers.clientTomlExistsInProject(paths),
		copyLocalTomlToProject: () => tomlHelpers.copyLocalClientTomlToProject(paths),
		copyProjectTomlLocally: () => tomlHelpers.copyProjectClientTomlLocally(paths),
	};
}

export function createServerCommonOps(
	waspDir: string,
	paths: tomlHelpers.TomlFilePaths,
): CommonOps {
	return {
		waspDir,
		paths,
		cdToBuildDir: () => cdToServerBuildDir(waspDir),
		tomlExistsInProject: () => tomlHelpers.serverTomlExistsInProject(paths),
		copyLocalTomlToProject: () => tomlHelpers.copyLocalServerTomlToProject(paths),
		copyProjectTomlLocally: () => tomlHelpers.copyProjectServerTomlLocally(paths),
	};
}

export enum ContextOption {
	Client = 'client',
	Server = 'server',
}

export function getCommonOps(context: ContextOption, waspDir: string, paths: tomlHelpers.TomlFilePaths): CommonOps {
	const commonOps: Record<ContextOption, CommonOps> = {
		[ContextOption.Client]: createClientCommonOps(waspDir, paths),
		[ContextOption.Server]: createServerCommonOps(waspDir, paths),
	};
	return commonOps[context];
}
