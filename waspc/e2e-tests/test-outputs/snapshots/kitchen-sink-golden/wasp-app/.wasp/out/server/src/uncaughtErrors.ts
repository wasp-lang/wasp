import { reportUnexpectedServerError } from "wasp/server/errors";

import { onServerError } from "../../../../src/features/errors/serverOnError"
import { registerServerOnErrorFn } from "wasp/server/errors";
import type { ServerOnErrorFn } from "wasp/server";

// We register the function here, so it also receives the errors thrown while
// the rest of the server's code loads.
registerServerOnErrorFn(onServerError satisfies ServerOnErrorFn);

// Node.js prints these errors and exits. We report them first, and still exit
// because the server can be in a broken state after them.
// https://nodejs.org/api/process.html#event-uncaughtexception
process.on("uncaughtException", (error: unknown) => {
  void exitAfterReporting(error, "uncaughtException");
});
process.on("unhandledRejection", (reason: unknown) => {
  void exitAfterReporting(reason, "unhandledRejection");
});

async function exitAfterReporting(
  error: unknown,
  event: "uncaughtException" | "unhandledRejection",
): Promise<void> {
  await reportUnexpectedServerError(error, { source: "process", event });
  process.exit(1);
}
