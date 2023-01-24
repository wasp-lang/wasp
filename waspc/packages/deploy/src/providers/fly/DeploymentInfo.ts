import { ITomlFilePaths } from './helpers/tomlFileHelpers.js'
import { IGlobalOptions } from './GlobalOptions.js'

export type IDeploymentInfo = Readonly<{
  baseName: string
  region?: string
  options: IGlobalOptions
  tomlFiles: ITomlFilePaths
  clientName: string
  clientUrl: string
  serverName: string
  serverUrl: string
  dbName: string
}>

export function createDeploymentInfo(
  baseName: string,
  region: string | undefined,
  options: IGlobalOptions,
  tomlFiles: ITomlFilePaths
): IDeploymentInfo {
  return {
    baseName,
    region,
    options,
    tomlFiles,
    clientName: `${baseName}-client`,
    clientUrl: `https://${baseName}-client.fly.dev`,
    serverName: `${baseName}-server`,
    serverUrl: `https://${baseName}-server.fly.dev`,
    dbName: `${baseName}-db`
  }
}
