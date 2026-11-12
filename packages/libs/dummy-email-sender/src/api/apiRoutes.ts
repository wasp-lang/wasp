import type { Response, Router } from "express";

import type { DevMailbox } from "../core/devMailbox.js";
import { streamMailboxEvents } from "./events.js";
import {
  getBody,
  InvalidInputError,
  parseFilter,
  parseOptionalPositiveInteger,
  parseTimeoutMs,
} from "./inputParsing.js";

/**
 * Reads are `GET`s, actions are `POST /api/<noun>.<verb>` with a JSON body.
 * The router must parse JSON bodies.
 */
export function registerApiRoutes(router: Router, mailbox: DevMailbox): void {
  router.get("/api/emails", (req, res) => {
    const filter = parseFilter(req.query);
    const limit = parseOptionalPositiveInteger(req.query.limit);
    if (filter instanceof InvalidInputError) {
      return sendError(res, 400, "invalid_filter", filter.message);
    }
    if (limit instanceof InvalidInputError) {
      return sendError(res, 400, "invalid_limit", limit.message);
    }
    res.json({ emails: mailbox.list(filter, limit) });
  });

  router.get("/api/events", (req, res) => {
    streamMailboxEvents(req, res, mailbox);
  });

  router.get("/api/emails/:id", (req, res) => {
    const email = mailbox.get(req.params.id);
    if (email === undefined) {
      return sendEmailNotFound(res, req.params.id);
    }
    res.json({ email });
  });

  router.get("/api/emails/:id/html", (req, res) => {
    const email = mailbox.get(req.params.id);
    if (email === undefined) {
      return sendEmailNotFound(res, req.params.id);
    }
    res
      .setHeader("Content-Security-Policy", "script-src 'none'")
      .type("html")
      .send(email.html);
  });

  router.post("/api/emails.waitFor", async (req, res) => {
    const body = getBody(req);
    const filter = parseFilter(body);
    const timeoutMs = parseTimeoutMs(body.timeoutMs);
    if (filter instanceof InvalidInputError) {
      return sendError(res, 400, "invalid_filter", filter.message);
    }
    if (timeoutMs instanceof InvalidInputError) {
      return sendError(res, 400, "invalid_timeout", timeoutMs.message);
    }

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
      return sendError(
        res,
        408,
        "timeout",
        "No matching email arrived before the timeout.",
      );
    }
    res.json({ email });
  });

  router.post("/api/emails.delete", (req, res) => {
    const { id } = getBody(req);
    if (typeof id !== "string") {
      return sendError(res, 400, "invalid_id", "`id` must be a string.");
    }
    if (!mailbox.delete(id)) {
      return sendEmailNotFound(res, id);
    }
    res.json({ ok: true });
  });

  router.post("/api/emails.clear", (_req, res) => {
    mailbox.clear();
    res.json({ ok: true });
  });
}

function sendEmailNotFound(res: Response, id: string) {
  return sendError(res, 404, "email_not_found", `No email with id ${id}.`);
}

function sendError(
  res: Response,
  status: number,
  code: string,
  message: string,
) {
  res.status(status).json({ error: { code, message } });
}
