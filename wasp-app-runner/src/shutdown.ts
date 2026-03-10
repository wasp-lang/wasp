import { setTimeout as delay } from "node:timers/promises";
import { createLogger } from "./logging.js";

const logger = createLogger("shutdown-controller");

const controller = new AbortController();

export const shutdownSignal = controller.signal;

export const shutdownPromise = new Promise<void>((resolve) =>
  shutdownSignal.addEventListener("abort", () => resolve()),
);

process.on("SIGINT", () => {
  logger.info("Received SIGINT");
  controller.abort("SIGINT");
});
process.on("SIGTERM", () => {
  logger.info("Received SIGTERM");
  controller.abort("SIGTERM");
});

shutdownSignal.addEventListener("abort", () => {
  logger.info("Shutdown triggered");

  // Force shutdown after 3 seconds, in case something is hanging.
  // This is a last resort, and should be enough time for most things to clean up.
  delay(3_000, undefined, {
    // We don't wait Node to wait for this timeout to complete before exiting,
    // it would be counterproductive, so we set `ref: false`.
    ref: false,
  }).then(forceShutdown);
});

function forceShutdown() {
  logger.error("Forcing shutdown");
  process.exit(1);
}
