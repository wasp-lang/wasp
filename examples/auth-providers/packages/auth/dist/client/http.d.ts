/**
 * The error the forms and actions throw, shaped like Wasp's client
 * `WaspHttpError`: `message` is the server's message, `data` the whole
 * response body -- so `error.data?.data?.message` reads the detail exactly
 * as the in-tree forms did.
 */
export declare class WaspAuthClientError extends Error {
    statusCode: number;
    data: unknown;
    constructor(statusCode: number, message: string, data: unknown);
}
/**
 * A POST as the signed-in user. Wasp attaches the credential; this package
 * never sees it, and can only spend it on its own routes.
 */
export declare function postAsUser<T = Record<string, unknown>>(url: string, body: unknown): Promise<T>;
export declare function post<T = Record<string, unknown>>(url: string, body: unknown, fetch?: (url: string, init: RequestInit) => Promise<Response>): Promise<T>;
