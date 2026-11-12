{{={= =}=}}
import * as path from "node:path";

import { DevMailbox } from "@wasp/lib-dummy-email-sender/node";

export { registerDevMailboxRoutes } from "@wasp/lib-dummy-email-sender/node";

// PRIVATE API
export const devMailboxRoutePath = "{= devMailboxRoutePath =}";

// PRIVATE API
// The server and `db seed` both run from the project dir, so they share it.
export const devMailbox = new DevMailbox({
  directoryPath: path.resolve("{= devMailboxDirFromWaspProjectDir =}"),
  maxMessages: {= maxMessages =},
});
