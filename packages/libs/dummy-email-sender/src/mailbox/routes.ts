import * as path from "node:path";

import type { Request, Response, Router } from "express";

import type { DevMailbox, DevMailboxEvent } from "./devMailbox.js";
import { makeInboxPage } from "./inboxPage.js";
import {
  DEV_MAILBOX_SEARCH_FIELDS,
  type DevMailboxMessageFilter,
  type DevMailboxSearchField,
} from "./messageFilter.js";

const DEFAULT_WAIT_TIMEOUT_MS = 10_000;
const MAX_WAIT_TIMEOUT_MS = 5 * 60_000;
const SSE_HEARTBEAT_INTERVAL_MS = 15_000;

/**
 * Registers the dev mailbox UI and API on the router. Reads are resource
 * style `GET`s, actions are RPC style `POST /api/<noun>.<verb>` calls with a
 * JSON body. Expects the router to parse JSON bodies.
 */
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
        directoryPath: getDisplayPath(mailbox.directoryPath),
      }),
    );
  });

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
    streamEvents(req, res, mailbox);
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

function streamEvents(req: Request, res: Response, mailbox: DevMailbox) {
  res.writeHead(200, {
    "Content-Type": "text/event-stream",
    "Cache-Control": "no-cache",
    Connection: "keep-alive",
  });
  res.write("retry: 1000\n\n");

  const sendEvent = (event: DevMailboxEvent) => {
    res.write(`event: ${event.type}\ndata: ${JSON.stringify(event)}\n\n`);
  };
  const unsubscribe = mailbox.subscribe(sendEvent);
  const heartbeat = setInterval(
    () => res.write(": heartbeat\n\n"),
    SSE_HEARTBEAT_INTERVAL_MS,
  );

  req.on("close", () => {
    clearInterval(heartbeat);
    unsubscribe();
  });
}

/** Relative to the Wasp project dir, where the server runs. */
function getDisplayPath(directoryPath: string): string {
  const relativePath = path.relative(process.cwd(), directoryPath);
  return relativePath.startsWith("..") ? directoryPath : `${relativePath}/`;
}

class InvalidInputError extends Error {}

type Input = Record<string, unknown>;

function getBody(req: Request): Input {
  return typeof req.body === "object" && req.body !== null ? req.body : {};
}

function parseFilter(
  input: Input,
): DevMailboxMessageFilter | InvalidInputError {
  const filter: DevMailboxMessageFilter = {};
  for (const key of ["after", "search"] as const) {
    const value = input[key];
    if (value === undefined || value === "") {
      continue;
    }
    if (typeof value !== "string") {
      return new InvalidInputError(`\`${key}\` must be a string.`);
    }
    filter[key] = value;
  }

  const searchIn = parseSearchIn(input.searchIn);
  if (searchIn instanceof InvalidInputError) {
    return searchIn;
  }
  if (searchIn !== undefined) {
    filter.searchIn = searchIn;
  }
  return filter;
}

/** Accepts an array, or a comma-separated string in query parameters. */
function parseSearchIn(
  value: unknown,
): DevMailboxSearchField[] | undefined | InvalidInputError {
  if (value === undefined || value === "") {
    return undefined;
  }
  const fields = typeof value === "string" ? value.split(",") : value;
  const isValid =
    Array.isArray(fields) &&
    fields.length > 0 &&
    fields.every((field) =>
      (DEV_MAILBOX_SEARCH_FIELDS as readonly unknown[]).includes(field),
    );
  if (!isValid) {
    return new InvalidInputError(
      `\`searchIn\` must list some of: ${DEV_MAILBOX_SEARCH_FIELDS.join(", ")}.`,
    );
  }
  return fields as DevMailboxSearchField[];
}

function parseOptionalPositiveInteger(
  value: unknown,
): number | undefined | InvalidInputError {
  if (value === undefined || value === "") {
    return undefined;
  }
  const number = typeof value === "string" ? Number(value) : value;
  if (typeof number !== "number" || !Number.isInteger(number) || number < 1) {
    return new InvalidInputError(
      `Expected a positive integer, got ${JSON.stringify(value)}.`,
    );
  }
  return number;
}

function parseTimeoutMs(value: unknown): number | InvalidInputError {
  const timeoutMs = parseOptionalPositiveInteger(value);
  if (timeoutMs instanceof InvalidInputError || timeoutMs === undefined) {
    return timeoutMs ?? DEFAULT_WAIT_TIMEOUT_MS;
  }
  if (timeoutMs > MAX_WAIT_TIMEOUT_MS) {
    return new InvalidInputError(
      `\`timeoutMs\` must be at most ${MAX_WAIT_TIMEOUT_MS}, got ${timeoutMs}.`,
    );
  }
  return timeoutMs;
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
