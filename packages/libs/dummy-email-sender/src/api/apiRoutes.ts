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
  router.get("/api/messages", (req, res) => {
    const filter = parseFilter(req.query);
    const limit = parseOptionalPositiveInteger(req.query.limit);
    if (filter instanceof InvalidInputError) {
      return sendError(res, 400, "invalid_filter", filter.message);
    }
    if (limit instanceof InvalidInputError) {
      return sendError(res, 400, "invalid_limit", limit.message);
    }
    res.json({ messages: mailbox.list(filter, limit) });
  });

  router.get("/api/events", (req, res) => {
    streamMailboxEvents(req, res, mailbox);
  });

  router.get("/api/messages/:id", (req, res) => {
    const message = mailbox.get(req.params.id);
    if (message === undefined) {
      return sendMessageNotFound(res, req.params.id);
    }
    res.json({ message });
  });

  router.get("/api/messages/:id/html", (req, res) => {
    const message = mailbox.get(req.params.id);
    if (message === undefined) {
      return sendMessageNotFound(res, req.params.id);
    }
    res
      .setHeader("Content-Security-Policy", "script-src 'none'")
      .type("html")
      .send(message.html);
  });

  router.post("/api/messages.waitFor", async (req, res) => {
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
    const message = await mailbox.waitFor(filter, {
      timeoutMs,
      signal: abortController.signal,
    });
    if (res.writableEnded || abortController.signal.aborted) {
      return;
    }
    if (message === undefined) {
      return sendError(
        res,
        408,
        "timeout",
        "No matching message arrived before the timeout.",
      );
    }
    res.json({ message });
  });

  router.post("/api/messages.delete", (req, res) => {
    const { id } = getBody(req);
    if (typeof id !== "string") {
      return sendError(res, 400, "invalid_id", "`id` must be a string.");
    }
    if (!mailbox.delete(id)) {
      return sendMessageNotFound(res, id);
    }
    res.json({ ok: true });
  });

  router.post("/api/messages.clear", (_req, res) => {
    mailbox.clear();
    res.json({ ok: true });
  });
}

function sendMessageNotFound(res: Response, id: string) {
  return sendError(res, 404, "message_not_found", `No message with id ${id}.`);
}

function sendError(
  res: Response,
  status: number,
  code: string,
  message: string,
) {
  res.status(status).json({ error: { code, message } });
}
