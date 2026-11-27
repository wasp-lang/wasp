// Node (Express-style) <-> web-standard Request/Response, so the MCP routes
// mount as one middleware on the Wasp server's Express app.
import type { IncomingMessage, ServerResponse } from "node:http";
import { Readable } from "node:stream";

type NodeRequest = IncomingMessage & { originalUrl?: string };
type Next = (err?: unknown) => void;

/**
 * The request as a web-standard Request. The body is streamed, not buffered,
 * so a request that fails authentication is never read, and the reader (the
 * MCP SDK or the OAuth routes) caps its size.
 */
export function toWebRequest(
  req: IncomingMessage & { originalUrl?: string },
  res: ServerResponse,
  baseUrl: string,
): Request {
  const headers = new Headers();
  for (const [name, value] of Object.entries(req.headers)) {
    if (value !== undefined)
      headers.set(name, Array.isArray(value) ? value.join(", ") : value);
  }
  // Aborts when the client goes away, so the SDK closes streams it holds open.
  const aborted = new AbortController();
  res.on("close", () => aborted.abort());

  const hasBody =
    req.method !== "GET" && req.method !== "HEAD" && req.method !== "OPTIONS";
  return new Request(`${baseUrl}${req.originalUrl ?? req.url ?? "/"}`, {
    method: req.method,
    headers,
    body: hasBody
      ? (Readable.toWeb(req) as ReadableStream<Uint8Array>)
      : undefined,
    // Required by Node's fetch for a streamed body.
    duplex: "half",
    signal: aborted.signal,
  } as RequestInit);
}

/**
 * Streams the response. Buffering would break 2026-07-28 clients: some
 * responses are SSE streams that stay open.
 */
export async function sendWebResponse(
  res: ServerResponse,
  response: Response,
): Promise<void> {
  res.statusCode = response.status;
  response.headers.forEach((value, name) => res.setHeader(name, value));
  if (!response.body) {
    res.end();
    return;
  }
  res.flushHeaders();
  const reader = response.body.getReader();
  res.on("close", () => void reader.cancel().catch(() => {}));
  try {
    for (;;) {
      const { done, value } = await reader.read();
      if (done) break;
      res.write(value);
    }
  } finally {
    res.end();
  }
}

export function toNodeMiddleware(
  http: {
    handle: (request: Request) => Promise<Response | null>;
    matches: (pathname: string) => boolean;
  },
  baseUrl: string,
) {
  return async (
    req: NodeRequest,
    res: ServerResponse,
    next: Next,
  ): Promise<void> => {
    // Leave other routes alone, including their request bodies.
    const pathname = new URL(req.originalUrl ?? req.url ?? "/", baseUrl)
      .pathname;
    if (!http.matches(pathname)) return next();
    try {
      const response = await http.handle(toWebRequest(req, res, baseUrl));
      if (!response) return next();
      await sendWebResponse(res, response);
    } catch (e) {
      if (res.headersSent) {
        res.end();
        return;
      }
      next(e);
    }
  };
}
