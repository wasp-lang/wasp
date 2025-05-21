import { createHash } from "crypto";
import { Branded } from "./types.js";
import type { PathToApp } from "./args.js";
import type { AppName } from "./waspCli.js";

export type DbContainerName = Branded<string, "ContainerName">;
export type ServerBuildContainerName = Branded<
  string,
  "ServerBuildContainerName"
>;
export type ServerBuildImageName = Branded<string, "ServerBuildImageName">;

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

export function createAppSpecificServerBuildDockerNames({
  appName,
  pathToApp,
}: {
  appName: AppName;
  pathToApp: PathToApp;
}): {
  imageName: ServerBuildImageName;
  containerName: ServerBuildContainerName;
} {
  const prefix = createAppSpecificPrefix({
    appName,
    pathToApp,
  });
  return {
    imageName: `${prefix}-server` as ServerBuildImageName,
    containerName: `${prefix}-server-container` as ServerBuildContainerName,
  };
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
