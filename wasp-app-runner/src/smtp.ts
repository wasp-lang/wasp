import { createLogger } from "./logging.js";
import { startServer } from "./server-starter.js";

export async function startLocalSmtpServer(): Promise<Disposable> {
  return await startServer(
    createLogger("smtp-server"),
    {
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
    },
    () => Promise.resolve(),
  );
}
