import { reportUnexpectedServerError } from "wasp/server/errors";

import { onServerError } from "../../../../src/features/errors/serverOnError"
import { registerServerOnErrorFn } from "wasp/server/errors";
import type { ServerOnErrorFn } from "wasp/server";

// We register the function here, so it also receives the errors thrown while
// the rest of the server's code loads.
registerServerOnErrorFn(onServerError satisfies ServerOnErrorFn);

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
