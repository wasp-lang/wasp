import { createLogger } from "./logging.js";
import { commandSucceeds } from "./process.js";

export async function checkDependencies({
  signal,
}: {
  signal: AbortSignal;
}): Promise<void> {
  const logger = createLogger("check-dependencies");

  const dockerAvailable = await commandSucceeds({
    name: "check-docker",
    cmd: "docker",
    args: ["--version"],
    signal,
  });

  if (!dockerAvailable) {
    logger.fatal("Required command 'docker' not found. Please install it.");
  }
}
