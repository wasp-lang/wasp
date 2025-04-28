import { createHash } from "crypto";
import { Branded } from "./types.js";

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
  appName: string;
  pathToApp: string;
}): DbContainerName {
  const appSpecificPrefix = createAppSpecificAppSpecificPrefix({
    appName,
    pathToApp,
  });
  return `${appSpecificPrefix}-db` as DbContainerName;
}

export function createAppSpecificServerBuildDockerNames({
  appName,
  pathToApp,
}: {
  appName: string;
  pathToApp: string;
}): {
  imageName: ServerBuildImageName;
  containerName: ServerBuildContainerName;
} {
  const appSpecificPrefix = createAppSpecificAppSpecificPrefix({
    appName,
    pathToApp,
  });
  return {
    imageName: `${appSpecificPrefix}-server` as ServerBuildImageName,
    containerName:
      `${appSpecificPrefix}-server-container` as ServerBuildContainerName,
  };
}

function createAppSpecificAppSpecificPrefix({
  appName,
  pathToApp,
}: {
  appName: string;
  pathToApp: string;
}): string {
  const appPathHash = createHash("md5")
    .update(pathToApp)
    .digest("hex")
    .slice(0, 16);
  return `${appName}-${appPathHash}`.toLowerCase();
}
