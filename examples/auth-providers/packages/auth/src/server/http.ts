/**
 * The same wire shape Wasp's `HttpError` produces (`{ message, data }` with
 * the status code), so the package's forms and any user code reading auth
 * errors see exactly what the in-tree flows produced. Duck-typed on purpose:
 * the app's own `HttpError` class (thrown from user hooks) is a different
 * class identity, and `statusCode` is what both share.
 */
export class HttpError extends Error {
  public statusCode: number;
  public data: unknown;

  constructor(
    statusCode: number,
    message?: string,
    data?: Record<string, unknown>,
  ) {
    super(message);
    this.name = "HttpError";
    this.statusCode = statusCode;
    if (data) {
      this.data = data;
    }
  }
}

export function isHttpErrorLike(
  error: unknown,
): error is { statusCode: number; message: string; data?: unknown } {
  return (
    typeof error === "object" &&
    error !== null &&
    typeof (error as { statusCode?: unknown }).statusCode === "number"
  );
}

/**
 * What this package's route handlers receive: the standard `Request` Wasp
 * handed over, plus the parsed body and the path relative to the mount, so
 * a route reads `req.body` and `req.url` like a Node handler would.
 */
export type Req = {
  request: Request;
  method: string;
  /** Path and query relative to the handler's mount (`/username/login`). */
  url: string;
  headers: Record<string, string>;
  /** The JSON body, when there was one; `{}` otherwise. */
  body: unknown;
};

/**
 * What this package's route handlers write to: a Node-style response
 * builder that becomes the standard `Response` the dispatcher returns.
 */
export class Res {
  statusCode = 200;
  private readonly headers = new Headers();
  private body: string | null = null;

  setHeader(name: string, value: string | string[]): void {
    this.headers.delete(name);
    for (const v of Array.isArray(value) ? value : [value]) {
      this.headers.append(name, v);
    }
  }

  end(body?: string): void {
    this.body = body ?? null;
  }

  toResponse(): Response {
    return new Response(this.body, {
      status: this.statusCode,
      headers: this.headers,
    });
  }
}

export function json(res: Res, status: number, payload: unknown): void {
  res.statusCode = status;
  res.setHeader("Content-Type", "application/json");
  res.end(JSON.stringify(payload));
}

/**
 * A standard `Response` in a form that survives a JSON round trip: the OAuth
 * one-time code carries the sign-in's answer this way, so redeeming the code
 * replays it.
 */
export type SerializedResponse = {
  status: number;
  headers: Array<[string, string]>;
  body: string | null;
};

export async function serializeResponse(
  response: Response,
): Promise<SerializedResponse> {
  const headers: Array<[string, string]> = [];
  response.headers.forEach((value, name) => {
    if (name !== "set-cookie") headers.push([name, value]);
  });
  for (const cookie of response.headers.getSetCookie()) {
    headers.push(["set-cookie", cookie]);
  }
  return {
    status: response.status,
    headers,
    body: response.body === null ? null : await response.text(),
  };
}

/**
 * Writes the answer of a sign-in: whatever the credentials issuer decided the
 * client should receive (a bearer token in the body, a Set-Cookie header).
 */
export function sendSerializedResponse(
  res: Res,
  response: SerializedResponse,
): void {
  res.statusCode = response.status;
  const grouped = new Map<string, string[]>();
  for (const [name, value] of response.headers) {
    grouped.set(name, [...(grouped.get(name) ?? []), value]);
  }
  for (const [name, values] of grouped) {
    res.setHeader(name, values);
  }
  res.end(response.body ?? undefined);
}

export async function sendAuthResponse(
  res: Res,
  response: Response,
): Promise<void> {
  sendSerializedResponse(res, await serializeResponse(response));
}

export function redirect(res: Res, location: string): void {
  res.statusCode = 302;
  res.setHeader("Location", location);
  res.end();
}

export function getBody(req: Req): Record<string, unknown> {
  const body = req.body;
  return typeof body === "object" && body !== null
    ? (body as Record<string, unknown>)
    : {};
}

/**
 * The per-sign-in properties a login request may ask for. Only "remember me"
 * is the client's call; the lifetime stays the app's configuration.
 */
export function getSignInProperties(fields: Record<string, unknown>): {
  persistent?: boolean;
} {
  return fields.persistent === false ? { persistent: false } : {};
}

export function getUrl(req: Req): URL {
  return new URL(req.url, "http://placeholder");
}

export type RouteHandler = (req: Req, res: Res) => Promise<void> | void;

export type Route = {
  method: "GET" | "POST";
  path: string;
  handler: RouteHandler;
};

/**
 * A minimal dispatcher over the routes mounted at the handler's mount path:
 * standard `Request` in, `Response` out. Errors follow Wasp's error-handling
 * contract: anything carrying a `statusCode` (this package's `HttpError`, the
 * app's own `HttpError` thrown from a hook) answers with that status and
 * `{ message, data }`; everything else is a logged 500.
 */
export function makeDispatcher(
  routes: Route[],
  mountPath: string,
): (request: Request) => Promise<Response> {
  return async (request) => {
    const req = await toReq(request, mountPath);
    const res = new Res();
    const url = getUrl(req);
    const route = routes.find(
      (r) => r.method === req.method && r.path === url.pathname,
    );
    if (route === undefined) {
      json(res, 404, { message: "Not found." });
      return res.toResponse();
    }
    try {
      await route.handler(req, res);
    } catch (error) {
      if (isHttpErrorLike(error)) {
        json(res, error.statusCode, {
          message: error.message,
          ...(error.data !== undefined ? { data: error.data } : {}),
        });
        return res.toResponse();
      }
      console.error(error);
      json(res, 500, { message: "Internal server error" });
    }
    return res.toResponse();
  };
}

async function toReq(request: Request, mountPath: string): Promise<Req> {
  const url = new URL(request.url);
  const path = url.pathname.startsWith(mountPath)
    ? url.pathname.slice(mountPath.length) || "/"
    : url.pathname;
  const headers: Record<string, string> = {};
  request.headers.forEach((value, name) => {
    headers[name] = value;
  });
  const isJson = (request.headers.get("content-type") ?? "").includes(
    "application/json",
  );
  const body =
    request.method === "GET" || request.method === "HEAD" || !isJson
      ? {}
      : await request.json().catch(() => ({}));
  return {
    request,
    method: request.method,
    url: path + url.search,
    headers,
    body,
  };
}
