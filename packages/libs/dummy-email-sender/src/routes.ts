import * as path from "node:path";

import type { Router } from "express";

import { registerApiRoutes } from "./api/apiRoutes.js";
import type { DevMailbox } from "./core/devMailbox.js";
import { makeInboxPage } from "./view/inboxPage.js";

/** Serves the inbox page at `/` and the API at `/api`. */
export function registerDevMailboxRoutes(
  router: Router,
  mailbox: DevMailbox,
  { appName }: { appName: string },
): void {
  router.get("/", (req, res) => {
    res.type("html").send(
      makeInboxPage({
        appName,
        apiBasePath: `${req.baseUrl}/api`,
        directoryPath: getPathRelativeToCwd(mailbox.directoryPath),
      }),
    );
  });
  registerApiRoutes(router, mailbox);
}

function getPathRelativeToCwd(directoryPath: string): string {
  const relativePath = path.relative(process.cwd(), directoryPath);
  return relativePath.startsWith("..") ? directoryPath : `${relativePath}/`;
}
