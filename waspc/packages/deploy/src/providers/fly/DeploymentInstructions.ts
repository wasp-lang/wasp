import { CommonCmdOptions } from "./CommonCmdOptions.js";
import { TomlFilePaths } from "./tomlFile.js";

export type DeploymentInstructions<CmdOptions extends CommonCmdOptions> =
  Readonly<{
    baseName: string;
    region?: string;
    cmdOptions: CmdOptions;
    tomlFilePaths: TomlFilePaths;
    clientName: string;
    clientUrl: string;
    serverName: string;
    serverUrl: string;
    dbName: string;
  }>;

export function createDeploymentInstructions<
  CmdOptions extends CommonCmdOptions,
>(
  baseName: string,
  region: string | undefined,
  cmdOptions: CmdOptions,
  tomlFilePaths: TomlFilePaths,
): DeploymentInstructions<CmdOptions> {
  return Object.freeze({
    baseName,
    region,
    cmdOptions,
    tomlFilePaths,
    clientName: `${baseName}-client`,
    clientUrl: `https://${baseName}-client.fly.dev`,
    serverName: `${baseName}-server`,
    serverUrl: `https://${baseName}-server.fly.dev`,
    dbName: `${baseName}-db`,
  });
}
