import { type AxiosInstance, type AxiosError } from 'axios';
export declare const api: AxiosInstance;
export declare function setSessionId(sessionId: string): void;
export declare function getSessionId(): string | null;
export declare function clearSessionId(): void;
export declare function removeLocalUserData(): void;
/**
 * Takes an error returned by the app's API (as returned by axios), and transforms into a more
 * standard format to be further used by the client. It is also assumed that given API
 * error has been formatted as implemented by HttpError on the server.
 */
export declare function handleApiError<T extends AxiosError<{
    message?: string;
    data?: unknown;
}>>(error: T): T | WaspHttpError;
declare class WaspHttpError extends Error {
    statusCode: number;
    data: unknown;
    constructor(statusCode: number, message: string, data: unknown);
}
export {};
//# sourceMappingURL=index.d.ts.map