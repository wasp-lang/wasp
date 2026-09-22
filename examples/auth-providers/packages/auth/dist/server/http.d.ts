/**
 * The same wire shape Wasp's `HttpError` produces (`{ message, data }` with
 * the status code), so the package's forms and any user code reading auth
 * errors see exactly what the in-tree flows produced. Duck-typed on purpose:
 * the app's own `HttpError` class (thrown from user hooks) is a different
 * class identity, and `statusCode` is what both share.
 */
export declare class HttpError extends Error {
    statusCode: number;
    data: unknown;
    constructor(statusCode: number, message?: string, data?: Record<string, unknown>);
}
export declare function isHttpErrorLike(error: unknown): error is {
    statusCode: number;
    message: string;
    data?: unknown;
};
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
export declare class Res {
    statusCode: number;
    private readonly headers;
    private body;
    setHeader(name: string, value: string | string[]): void;
    end(body?: string): void;
    toResponse(): Response;
}
export declare function json(res: Res, status: number, payload: unknown): void;
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
export declare function serializeResponse(response: Response): Promise<SerializedResponse>;
/**
 * Writes the answer of a sign-in: whatever the credentials issuer decided the
 * client should receive (a bearer token in the body, a Set-Cookie header).
 */
export declare function sendSerializedResponse(res: Res, response: SerializedResponse): void;
export declare function sendAuthResponse(res: Res, response: Response): Promise<void>;
export declare function redirect(res: Res, location: string): void;
export declare function getBody(req: Req): Record<string, unknown>;
/**
 * The per-sign-in properties a login request may ask for. Only "remember me"
 * is the client's call; the lifetime stays the app's configuration.
 */
export declare function getSignInProperties(fields: Record<string, unknown>): {
    persistent?: boolean;
};
export declare function getUrl(req: Req): URL;
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
export declare function makeDispatcher(routes: Route[], mountPath: string): (request: Request) => Promise<Response>;
