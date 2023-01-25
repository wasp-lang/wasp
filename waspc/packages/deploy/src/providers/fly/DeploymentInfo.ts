import { TomlFilePaths } from './helpers/tomlFileHelpers.js'
import { GlobalOptions } from './GlobalOptions.js'

export type DeploymentInfo = Readonly<{
  baseName: string
  region?: string
  options: GlobalOptions
  tomlFiles: TomlFilePaths
  clientName: string
  clientUrl: string
  serverName: string
  serverUrl: string
  dbName: string
}>

export function createDeploymentInfo(
  baseName: string,
  region: string | undefined,
  options: GlobalOptions,
  tomlFiles: TomlFilePaths
): DeploymentInfo {
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
