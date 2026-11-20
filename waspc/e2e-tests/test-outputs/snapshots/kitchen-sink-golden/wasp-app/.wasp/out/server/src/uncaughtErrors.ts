import { listenForUncaughtServerErrors } from "wasp/server/errors";

import { onServerError } from "../../../../src/features/errors/serverOnError"
import { registerServerOnErrorFn } from "wasp/server/errors";
import type { ServerOnErrorFn } from "wasp/server";

// We register the function here, so it also receives the errors thrown while
// the rest of the server's code loads.
registerServerOnErrorFn(onServerError satisfies ServerOnErrorFn);

listenForUncaughtServerErrors();
