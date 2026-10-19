import { cd, tmpdir } from "zx";
import { WaspProjectDir } from "../../common/brandedTypes.js";
import { getServerDeploymentDir } from "../../common/waspProject.js";
import {
  clientTomlExistsInProject,
  copyLocalClientTomlToProject,
  copyLocalServerTomlToProject,
  copyProjectClientTomlLocally,
  copyProjectServerTomlLocally,
  serverTomlExistsInProject,
  TomlFilePaths,
} from "./tomlFile.js";

export type CommonOps = Readonly<{
  waspProjectDir: string;
  paths: TomlFilePaths;
  cdToFlyctlWorkingDir: () => void;
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
    // The client deployment dir only exists after a deploy builds the client,
    // and flyctl only needs the fly.toml we copy there.
    cdToFlyctlWorkingDir: () => cd(tmpdir()),
    tomlExistsInProject: () => clientTomlExistsInProject(paths),
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
    cdToFlyctlWorkingDir: () => cd(getServerDeploymentDir(waspProjectDir)),
    tomlExistsInProject: () => serverTomlExistsInProject(paths),
    copyLocalTomlToProject: () => copyLocalServerTomlToProject(paths),
    copyProjectTomlLocally: () => copyProjectServerTomlLocally(paths),
  };
}
