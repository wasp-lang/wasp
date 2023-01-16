import { cdToClientBuildDir, cdToServerBuildDir } from "./helpers.js"
import * as tomlHelpers from "./tomlFileHelpers.js"

export interface ICommonOps {
  waspDir: string
  paths: tomlHelpers.ITomlFilePaths

  cdToBuildDir: () => void
  tomlExistsInProject: () => boolean
  copyLocalTomlToProject: () => void
  copyProjectTomlLocally: () => void
}

export class ClientCommonOps implements ICommonOps {
  waspDir: string
  paths: tomlHelpers.ITomlFilePaths

  constructor(waspDir: string, paths: tomlHelpers.ITomlFilePaths) {
    this.waspDir = waspDir
    this.paths = paths
  }

  cdToBuildDir = () => cdToClientBuildDir(this.waspDir)
  tomlExistsInProject = () => tomlHelpers.clientTomlExistsInProject(this.paths)
  copyLocalTomlToProject = () => tomlHelpers.copyLocalClientTomlToProject(this.paths)
  copyProjectTomlLocally = () => tomlHelpers.copyProjectClientTomlLocally(this.paths)
}

export class ServerCommonOps implements ICommonOps {
  waspDir: string
  paths: tomlHelpers.ITomlFilePaths

  constructor(waspDir: string, paths: tomlHelpers.ITomlFilePaths) {
    this.waspDir = waspDir
    this.paths = paths
  }

  cdToBuildDir = () => cdToServerBuildDir(this.waspDir)
  tomlExistsInProject = () => tomlHelpers.serverTomlExistsInProject(this.paths)
  copyLocalTomlToProject = () => tomlHelpers.copyLocalServerTomlToProject(this.paths)
  copyProjectTomlLocally = () => tomlHelpers.copyProjectServerTomlLocally(this.paths)
}
