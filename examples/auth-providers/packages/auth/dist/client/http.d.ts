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
 * A POST to one of this package's own routes, through the fetch Wasp hands
 * the client half. Wasp attaches the current credential when there is one
 * (this package never sees it) and picks the credentials mode: cookies ride
 * along only when the app actually uses a cookie credential, so an app with
 * a custom CORS setup keeps working.
 */
export declare function post<T = Record<string, unknown>>(url: string, body: unknown): Promise<T>;
