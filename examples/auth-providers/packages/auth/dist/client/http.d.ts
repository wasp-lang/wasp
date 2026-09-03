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
export declare function post<T = Record<string, unknown>>(path: string, body: unknown): Promise<T>;
