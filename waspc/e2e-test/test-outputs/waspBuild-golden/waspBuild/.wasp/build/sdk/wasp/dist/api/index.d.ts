import { type AxiosError } from 'axios';
export declare const api: import("axios").AxiosInstance;
export declare function setSessionId(sessionId: string): void;
export declare function getSessionId(): string | undefined;
export declare function clearSessionId(): void;
export declare function removeLocalUserData(): void;
/**
 * Takes an error returned by the app's API (as returned by axios), and transforms into a more
 * standard format to be further used by the client. It is also assumed that given API
 * error has been formatted as implemented by HttpError on the server.
 */
export declare function handleApiError(error: AxiosError<{
    message?: string;
    data?: unknown;
}>): void;
