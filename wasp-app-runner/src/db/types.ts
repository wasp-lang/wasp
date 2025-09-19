import type { PathToApp } from "../args.js";
import type { AppName } from "../waspCli.js";

export type SetupDbFn = (options: {
  appName: AppName;
  pathToApp: PathToApp;
  dbImage?: string;
}) => Promise<{
  dbEnvVars: { [envVarName: string]: string };
}>;
