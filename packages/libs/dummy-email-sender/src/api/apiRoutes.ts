import type { Router } from "express";

import type { DevMailbox } from "../core/devMailbox.js";
import type { DevMailboxEmail } from "../core/email.js";
import { ApiError, withApiErrorResponses } from "./apiError.js";
import { DEV_MAILBOX_API_BASE_PATH, devMailboxApiPaths } from "./apiPaths.js";
import { streamMailboxEvents } from "./events.js";
import {
  getJsonBody,
  parseEmailId,
  parseFilter,
  parseLimit,
  parseTimeoutMs,
} from "./inputParsing.js";

/**
 * Reads are `GET`s, actions are `POST /api/<noun>.<verb>` with a JSON body.
 */
export function registerApiRoutes(router: Router, mailbox: DevMailbox): void {
  const route = (path: string) => DEV_MAILBOX_API_BASE_PATH + path;

  router.get(
    route(devMailboxApiPaths.emails),
    withApiErrorResponses((req, res) => {
      const filter = parseFilter(req.query);
      const limit = parseLimit(req.query.limit);
      res.json({ emails: mailbox.list(filter, limit) });
    }),
  );

  router.get(route(devMailboxApiPaths.events), (req, res) => {
    streamMailboxEvents(req, res, mailbox);
  });

  router.get(
    route(devMailboxApiPaths.email(":id")),
    withApiErrorResponses((req, res) => {
      res.json({
        email: getEmailOrThrow(mailbox, parseEmailId(req.params.id)),
      });
    }),
  );

  router.get(
    route(devMailboxApiPaths.emailHtml(":id")),
    withApiErrorResponses((req, res) => {
      const email = getEmailOrThrow(mailbox, parseEmailId(req.params.id));
      res
        .setHeader("Content-Security-Policy", "script-src 'none'")
        .type("html")
        .send(email.html);
    }),
  );

  router.post(
    route(devMailboxApiPaths.waitForEmail),
    withApiErrorResponses(async (req, res) => {
      const body = getJsonBody(req);
      const filter = parseFilter(body);
      const timeoutMs = parseTimeoutMs(body.timeoutMs);

      const abortController = new AbortController();
      res.on("close", () => abortController.abort());
      const email = await mailbox.waitFor(filter, {
        timeoutMs,
        signal: abortController.signal,
      });
      if (res.writableEnded || abortController.signal.aborted) {
        return;
      }
      if (email === undefined) {
        throw new ApiError(
          408,
          "timeout",
          "No matching email arrived before the timeout.",
        );
      }
      res.json({ email });
    }),
  );

  router.post(
    route(devMailboxApiPaths.deleteEmail),
    withApiErrorResponses((req, res) => {
      const id = parseEmailId(getJsonBody(req).id);
      if (!mailbox.delete(id)) {
        throw emailNotFound(id);
      }
      res.json({ ok: true });
    }),
  );

  router.post(route(devMailboxApiPaths.clearEmails), (_req, res) => {
    mailbox.clear();
    res.json({ ok: true });
  });
}

function getEmailOrThrow(mailbox: DevMailbox, id: string): DevMailboxEmail {
  const email = mailbox.get(id);
  if (email === undefined) {
    throw emailNotFound(id);
  }
  return email;
}

function emailNotFound(id: string): ApiError {
  return new ApiError(404, "email_not_found", `No email with id ${id}.`);
}
