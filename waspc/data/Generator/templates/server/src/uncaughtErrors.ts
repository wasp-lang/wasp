{{={= =}=}}
import { reportUnexpectedServerError } from "wasp/server/errors";

{=# onErrorFn.isDefined =}
{=& onErrorFn.importStatement =}
import { registerServerOnErrorFn } from "wasp/server/errors";
import type { ServerOnErrorFn } from "wasp/server";

// We register the function here, so it also receives the errors thrown while
// the rest of the server's code loads.
registerServerOnErrorFn({= onErrorFn.importIdentifier =} satisfies ServerOnErrorFn);

{=/ onErrorFn.isDefined =}
// Node.js prints these errors and exits. We report them first, and still exit
// because the server can be in a broken state after them.
// https://nodejs.org/api/process.html#event-uncaughtexception
process.on("uncaughtException", exitAfterReporting);
process.on("unhandledRejection", exitAfterReporting);

async function exitAfterReporting(error: unknown): Promise<void> {
  await reportUnexpectedServerError(error, { source: "process" });
  process.exit(1);
}
