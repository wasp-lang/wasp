import { createHash } from "crypto";
import type { DockerImageName, PathToApp } from "./args.js";
import { createLogger } from "./logging.js";
import { spawnWithLog } from "./process.js";
import { Branded } from "./types.js";
import type { AppName } from "./waspCli.js";

export type DbContainerName = Branded<string, "ContainerName">;

const pullLogger = createLogger("docker-pull");

export async function pullDockerImage(image: DockerImageName): Promise<void> {
  pullLogger.info(`Pulling Docker image: ${image}...`);
  try {
    await spawnWithLog({
      name: "docker-pull",
      cmd: "docker",
      args: ["pull", image],
    });
  } catch {
    pullLogger.error(`Failed to pull Docker image: ${image}`);
    process.exit(1);
  }
}

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
