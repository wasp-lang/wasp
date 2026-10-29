import { CommonCmdOptions } from "./CommonCmdOptions.js";
import { TomlFilePaths } from "./tomlFile.js";

export type DeploymentInstructions<CmdOptions extends CommonCmdOptions> =
  Readonly<{
    baseName: string;
    region?: string;
    cmdOptions: CmdOptions;
    tomlFilePaths: TomlFilePaths;
    clientFlyAppName: string;
    serverFlyAppName: string;
    dbName: string;
  }>;

export function createDeploymentInstructions<
  CmdOptions extends CommonCmdOptions,
>({
  baseName,
  cmdOptions,
  tomlFilePaths,
  region,
}: {
  baseName: string;
  region?: string;
  cmdOptions: CmdOptions;
  tomlFilePaths: TomlFilePaths;
}): DeploymentInstructions<CmdOptions> {
  return Object.freeze({
    baseName,
    region,
    cmdOptions,
    tomlFilePaths,
    clientFlyAppName: `${baseName}-client`,
    serverFlyAppName: `${baseName}-server`,
    dbName: `${baseName}-db`,
  });
}
