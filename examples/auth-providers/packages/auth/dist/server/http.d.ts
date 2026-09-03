import type { Req, Res } from "./types.js";
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
export declare function json(res: Res, status: number, payload: unknown): void;
export declare function redirect(res: Res, location: string): void;
export declare function getBody(req: Req): Record<string, unknown>;
export declare function getUrl(req: Req): URL;
export type RouteHandler = (req: Req, res: Res) => Promise<void> | void;
export type Route = {
    method: "GET" | "POST";
    path: string;
    handler: RouteHandler;
};
/**
 * A minimal dispatcher over the routes mounted at the manifest's basePath.
 * Errors follow Wasp's error-handling contract: anything carrying a
 * `statusCode` (this package's `HttpError`, the app's own `HttpError`
 * thrown from a hook) answers with that status and `{ message, data }`;
 * everything else is a logged 500.
 */
export declare function makeDispatcher(routes: Route[]): (req: Req, res: Res) => Promise<void>;
