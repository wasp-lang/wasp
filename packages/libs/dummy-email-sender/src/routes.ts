import * as fs from "node:fs";
import * as path from "node:path";
import { fileURLToPath } from "node:url";

import type { Router } from "express";

import { registerApiRoutes } from "./api/apiRoutes.js";
import type { DevMailbox } from "./core/devMailbox.js";

// Next to this module both in `src/` and in the bundled `dist/`.
const VIEW_DIR_PATH = fileURLToPath(new URL("./view/", import.meta.url));

/**
 * Serves the inbox page at `/` and the API at `/api`. The router must parse
 * JSON bodies, e.g. with `express.json()`.
 */
export function registerDevMailboxRoutes(
  router: Router,
  mailbox: DevMailbox,
  { appName }: { appName: string },
): void {
  const inboxPage = fs
    .readFileSync(path.join(VIEW_DIR_PATH, "index.html"), "utf8")
    .replaceAll("%APP_NAME%", escapeHtml(appName))
    .replaceAll(
      "%DIRECTORY_PATH%",
      escapeHtml(getPathRelativeToCwd(mailbox.directoryPath)),
    );

  router.get("/", (req, res) => {
    // The page uses relative URLs, which need the trailing slash.
    const url = new URL(req.originalUrl, "http://localhost");
    if (!url.pathname.endsWith("/")) {
      res.redirect(url.pathname + "/" + url.search);
      return;
    }
    res.type("html").send(inboxPage);
  });
  for (const assetFileName of ["inbox.css", "inbox.js"]) {
    router.get(`/${assetFileName}`, (_req, res) => {
      res.sendFile(assetFileName, { root: VIEW_DIR_PATH });
    });
  }
  registerApiRoutes(router, mailbox);
}

function escapeHtml(value: string): string {
  return value
    .replace(/&/g, "&amp;")
    .replace(/</g, "&lt;")
    .replace(/>/g, "&gt;")
    .replace(/"/g, "&quot;");
}

function getPathRelativeToCwd(directoryPath: string): string {
  const relativePath = path.relative(process.cwd(), directoryPath);
  return relativePath.startsWith("..") ? directoryPath : `${relativePath}/`;
}
