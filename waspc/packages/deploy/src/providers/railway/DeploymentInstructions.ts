import { WaspCliExe, WaspProjectDir } from "../../common/brandedTypes.js";
import {
  ClientServiceName,
  DbServiceName,
  RailwayCliExe,
  RailwayProjectName,
  ServerServiceName,
} from "./brandedTypes.js";
import {
  generateRailwayClientServiceName,
  generateRailwayDbServiceName,
  generateRailwayServerServiceName,
} from "./railwayService/nameGenerator.js";

export type DeploymentInstructions<CmdOptions extends CommonCmdOptions> =
  Readonly<{
    projectName: RailwayProjectName;
    cmdOptions: CmdOptions;
    clientServiceName: ClientServiceName;
    serverServiceName: ServerServiceName;
    dbServiceName: DbServiceName;
  }>;

export function createDeploymentInstructions<
  CmdOptions extends CommonCmdOptions,
>(
  projectName: RailwayProjectName,
  cmdOptions: CmdOptions,
): DeploymentInstructions<CmdOptions> {
  return Object.freeze({
    projectName,
    cmdOptions,
    clientServiceName: generateRailwayClientServiceName(projectName),
    serverServiceName: generateRailwayServerServiceName(projectName),
    dbServiceName: generateRailwayDbServiceName(),
  });
}

export interface CommonCmdOptions {
  waspExe: WaspCliExe;
  railwayExe: RailwayCliExe;
  waspProjectDir: WaspProjectDir;
}

export interface SecretsOptions {
  serverSecret: string[];
  // There are client runtime secrets, they are available to the server serving the static files.
  // They are useless right now (might be useful when we go with SSR) since our client can't access them.
  clientSecret: string[];
}
