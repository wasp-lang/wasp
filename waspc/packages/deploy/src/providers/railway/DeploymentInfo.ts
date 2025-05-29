import { CommonOptions } from "./CommonOptions.js";

export type DeploymentInfo<CommandOptions extends CommonOptions> = Readonly<{
  baseName: string;
  options: CommandOptions;
  clientName: string;
  serverName: string;
  dbName: string;
}>;

export function createDeploymentInfo<CommandOptions extends CommonOptions>(
  baseName: string,
  options: CommandOptions,
): DeploymentInfo<CommandOptions> {
  return Object.freeze({
    baseName,
    options,
    clientName: `${baseName}-client`,
    serverName: `${baseName}-server`,
    dbName: `Postgres`,
  });
}
