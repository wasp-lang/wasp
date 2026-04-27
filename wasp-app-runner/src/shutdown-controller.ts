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
});
