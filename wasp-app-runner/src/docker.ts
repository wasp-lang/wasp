import { createHash } from "node:crypto";
import type { DockerImageName, PathToApp } from "./args.ts";
import { createLogger } from "./logging.ts";
import { spawnWithLog } from "./process.ts";
import type { Branded } from "./types.ts";
import type { AppName } from "./waspCli.ts";

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
