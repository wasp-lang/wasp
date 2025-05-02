import { exitCode } from "process";
import { spawnWithLog } from "./process.js";
import { log } from "./logging.js";

export async function startLocalSmtpServer(): Promise<void> {
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
      log("smtp-server", "error", `SMTP server exited with code ${exitCode}`);
      process.exit(1);
    }
  });
}
