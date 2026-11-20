import {
  reportFatalServerError,
  reportUnexpectedServerError,
} from "./report.js";

/**
 * Reports the errors that reach the Node.js process, e.g. an error thrown in
 * a timer or a rejected promise that nobody awaited.
 *
 * @see https://nodejs.org/api/process.html#event-uncaughtexception
 * @see https://nodejs.org/api/process.html#event-unhandledrejection
 */
export function listenForUncaughtServerErrors(): void {
  // Node.js prints an uncaught exception and exits. We report it first, and
  // still exit because the server can be in a broken state after it.
  process.on("uncaughtException", async (error: unknown) => {
    await reportFatalServerError(error, {
      source: "process",
      event: "uncaughtException",
    });
    process.exit(1);
  });

  // An unhandled rejection is usually a promise nobody awaited, which doesn't
  // break the server, so we report it and keep running.
  process.on("unhandledRejection", (reason: unknown) => {
    void reportUnexpectedServerError(reason, {
      source: "process",
      event: "unhandledRejection",
    });
  });
}
