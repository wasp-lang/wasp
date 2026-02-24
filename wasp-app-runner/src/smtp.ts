import { createLogger } from "./logging.js";
import { spawnWithLog } from "./process.js";

export async function startLocalSmtpServer(signal?: AbortSignal): Promise<void> {
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
    signal,
  }).catch((err) => {
    if (err?.name === "AbortError") {
      return;
    }
    logger.error(`SMTP server exited unexpectedly: ${err}`);
  });
}
