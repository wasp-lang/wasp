import { reportUnexpectedServerError } from "wasp/server/errors";

// Node.js prints these errors and exits. We report them first, and still exit
// because the server can be in a broken state after them.
// https://nodejs.org/api/process.html#event-uncaughtexception
process.on("uncaughtException", exitAfterReporting);
process.on("unhandledRejection", exitAfterReporting);

function exitAfterReporting(error: unknown): void {
  reportUnexpectedServerError(error, { source: "process" });
  process.exit(1);
}
