import { waitUntilHttpOnPort } from "./http.js";
import { createLogger } from "./logging.js";
import { startServer } from "./server-starter.js";

const MAILCRAB_WEB_UI_PORT = 1080;
const MAILCRAB_SMTP_PORT = 1025;

export async function startLocalSmtpServer(): Promise<Disposable> {
  return await startServer(
    createLogger("smtp-server"),
    {
      cmd: "docker",
      args: [
        "run",
        "--rm",
        "-p",
        `${MAILCRAB_WEB_UI_PORT}:1080`,
        "-p",
        `${MAILCRAB_SMTP_PORT}:1025`,
        "marlonb/mailcrab:latest",
      ],
    },
    () => waitUntilHttpOnPort(MAILCRAB_WEB_UI_PORT),
  );
}
