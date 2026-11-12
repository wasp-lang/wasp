import * as path from "node:path";

import { DevMailbox } from "@wasp/lib-dummy-email-sender/node";

export { registerDevMailboxRoutes } from "@wasp/lib-dummy-email-sender/node";

// PRIVATE API
export const devMailboxRoutePath = "/__wasp/mailbox";

// PRIVATE API
// Wasp runs the server and `db seed` from the Wasp project dir, so both share
// this directory.
export const devMailbox = new DevMailbox({
  directoryPath: path.resolve(".wasp/mailbox"),
  maxMessages: 500,
});
