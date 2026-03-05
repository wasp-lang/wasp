import { Process } from "./process.js";

export function startLocalSmtpServer(): AsyncDisposable {
  return new Process({
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
  })
    .log("smtp-server")
    .disposable();
}
