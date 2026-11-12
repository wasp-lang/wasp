import type { Request, Response } from "express";

import type { DevMailbox, DevMailboxEvent } from "../core/devMailbox.js";

const HEARTBEAT_INTERVAL_MS = 15_000;

export function streamMailboxEvents(
  req: Request,
  res: Response,
  mailbox: DevMailbox,
): void {
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
    HEARTBEAT_INTERVAL_MS,
  );

  req.on("close", () => {
    clearInterval(heartbeat);
    unsubscribe();
  });
}
