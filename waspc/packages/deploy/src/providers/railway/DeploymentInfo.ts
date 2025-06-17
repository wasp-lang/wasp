import { Branded } from "../../common/branded.js";
import { CommonOptions } from "./CommonOptions.js";

export type RailwayProjectName = Branded<string, "RailwayProjectName">;
export type ClientServiceName = Branded<string, "ClientServiceName">;
export type ServerServiceName = Branded<string, "ServerServiceName">;
export type DbServiceName = Branded<string, "DbServiceName">;

export type DeploymentInfo<CommandOptions extends CommonOptions> = Readonly<{
  projectName: RailwayProjectName;
  options: CommandOptions;
  clientServiceName: ClientServiceName;
  serverServiceName: ServerServiceName;
  dbServiceName: DbServiceName;
}>;

// Railway doesn't allow us to choose the database service name.
const hardcodedRailwayDbServiceName = "Postgres" as DbServiceName;

export function createDeploymentInfo<CommandOptions extends CommonOptions>(
  projectName: RailwayProjectName,
  options: CommandOptions,
): DeploymentInfo<CommandOptions> {
  return Object.freeze({
    projectName: projectName as RailwayProjectName,
    options,
    clientServiceName: `${projectName}-client` as ClientServiceName,
    serverServiceName: `${projectName}-server` as ServerServiceName,
    dbServiceName: hardcodedRailwayDbServiceName,
  });
}
