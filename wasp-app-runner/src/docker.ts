import { createHash } from "crypto";
import type { PathToApp } from "./args.js";
import { Branded } from "./types.js";
import type { AppName } from "./waspCli.js";

export type DbContainerName = Branded<string, "ContainerName">;

export function createAppSpecificDbContainerName({
  appName,
  pathToApp,
}: {
  appName: AppName;
  pathToApp: PathToApp;
}): DbContainerName {
  const prefix = createAppSpecificPrefix({
    appName,
    pathToApp,
  });
  return `${prefix}-db` as DbContainerName;
}

function createAppSpecificPrefix({
  appName,
  pathToApp,
}: {
  appName: AppName;
  pathToApp: PathToApp;
}): string {
  const appPathHash = createHash("md5")
    .update(pathToApp)
    .digest("hex")
    .slice(0, 16);
  return `${appName}-${appPathHash}`.toLowerCase();
}
