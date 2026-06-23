import type { DockerImageName } from "./args.ts";
import { pullDockerImage } from "./docker.ts";
import { createLogger } from "./logging.ts";
import { spawnWithLog } from "./process.ts";

const mailcrabImage = "marlonb/mailcrab:latest" as DockerImageName;

export async function startLocalSmtpServer(): Promise<void> {
  const logger = createLogger("smtp-server");

  await pullDockerImage(mailcrabImage);

  spawnWithLog({
    name: "smtp-server",
    cmd: "docker",
    args: ["run", "--rm", "-p", "1080:1080", "-p", "1025:1025", mailcrabImage],
  }).then(({ exitCode }) => {
    if (exitCode !== 0) {
      logger.error(`SMTP server exited with code ${exitCode}`);
      process.exit(1);
    }
  });
}
