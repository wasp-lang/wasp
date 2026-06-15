import type { DockerImageName, PathToApp } from "./args.js";
import {
  createAppSpecificContainerName,
  pullDockerImage,
  startContainer,
} from "./docker.js";
import type { ManagedService } from "./run.js";
import type { AppName } from "./waspCli.js";

const mailcrabImage = "marlonb/mailcrab:latest" as DockerImageName;

export async function startLocalSmtpServer({
  appName,
  pathToApp,
  signal,
}: {
  appName: AppName;
  pathToApp: PathToApp;
  signal: AbortSignal;
}): Promise<ManagedService> {
  await pullDockerImage(mailcrabImage, { signal });

  const containerName = createAppSpecificContainerName("smtp", {
    appName,
    pathToApp,
  });

  return startContainer({
    name: "smtp-server",
    containerName,
    image: mailcrabImage,
    dockerRunArgs: ["-p", "1080:1080", "-p", "1025:1025"],
    output: "log",
    signal,
  });
}
