import { cd } from "zx";
import { WaspProjectDir } from "../../common/brandedTypes.js";
import {
  getClientDeploymentDir,
  getServerDeploymentDir,
} from "../../common/waspProject.js";
import {
  doesClientTomlExistInProject,
  copyLocalClientTomlToProject,
  copyLocalServerTomlToProject,
  copyProjectClientTomlLocally,
  copyProjectServerTomlLocally,
  doesServerTomlExistInProject,
  TomlFilePaths,
} from "./tomlFile.js";

export type CommonOps = Readonly<{
  waspProjectDir: string;
  paths: TomlFilePaths;
  cdToDeploymentDir: () => void;
  tomlExistsInProject: () => boolean;
  copyLocalTomlToProject: () => void;
  copyProjectTomlLocally: () => void;
}>;

export enum ContextOption {
  Client = "client",
  Server = "server",
}

export function getCommonOps(
  context: ContextOption,
  waspProjectDir: WaspProjectDir,
  paths: TomlFilePaths,
): CommonOps {
  const commonOps: Record<ContextOption, CommonOps> = {
    [ContextOption.Client]: createClientCommonOps(waspProjectDir, paths),
    [ContextOption.Server]: createServerCommonOps(waspProjectDir, paths),
  };
  return commonOps[context];
}

function createClientCommonOps(
  waspProjectDir: WaspProjectDir,
  paths: TomlFilePaths,
): CommonOps {
  return {
    waspProjectDir,
    paths,
    cdToDeploymentDir: () => cd(getClientDeploymentDir(waspProjectDir)),
    tomlExistsInProject: () => doesClientTomlExistInProject(paths),
    copyLocalTomlToProject: () => copyLocalClientTomlToProject(paths),
    copyProjectTomlLocally: () => copyProjectClientTomlLocally(paths),
  };
}

function createServerCommonOps(
  waspProjectDir: WaspProjectDir,
  paths: TomlFilePaths,
): CommonOps {
  return {
    waspProjectDir,
    paths,
    cdToDeploymentDir: () => cd(getServerDeploymentDir(waspProjectDir)),
    tomlExistsInProject: () => doesServerTomlExistInProject(paths),
    copyLocalTomlToProject: () => copyLocalServerTomlToProject(paths),
    copyProjectTomlLocally: () => copyProjectServerTomlLocally(paths),
  };
}
