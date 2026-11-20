import { reportUnexpectedServerError } from "wasp/server/errors";

// Node.js prints an uncaught exception and exits. We report it first, and
// still exit because the server can be in a broken state after it.
// https://nodejs.org/api/process.html#event-uncaughtexception
process.on("uncaughtException", async (error: unknown) => {
  await reportUnexpectedServerError(error, {
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
