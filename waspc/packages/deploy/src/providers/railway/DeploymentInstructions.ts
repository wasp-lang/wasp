import { WaspCliExe, WaspProjectDir } from "../../common/brandedTypes.js";
import {
  ClientServiceName,
  DbServiceName,
  RailwayCliExe,
  RailwayProjectName,
  ServerServiceName,
} from "./brandedTypes.js";
import {
  createRailwayClientServiceName,
  createRailwayDbServiceName,
  createRailwayServerServiceName,
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
    clientServiceName: createRailwayClientServiceName(projectName),
    serverServiceName: createRailwayServerServiceName(projectName),
    dbServiceName: createRailwayDbServiceName(),
  });
}

// These common cmd options are similar to the ones for the Fly provider:
// We use both the Wasp CLI, Wasp project directory, and the provider CLI (Railway CLI).
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
