import { spawnWithLog } from "./process.js";
import { createLogger } from "./logging.js";

export async function startLocalSmtpServer(): Promise<void> {
  const logger = createLogger("smtp-server");
  spawnWithLog({
    name: "smtp-server",
    cmd: "docker",
    args: [
      "run",
      "--rm",
      "-p",
      "1080:1080",
      "-p",
      "1025:1025",
      "marlonb/mailcrab:latest",
    ],
  }).then(({ exitCode }) => {
    if (exitCode !== 0) {
      logger.error(`SMTP server exited with code ${exitCode}`);
      process.exit(1);
    }
  });
}
