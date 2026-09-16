import type { ExternalAuthProviderId } from '../auth/provider.js';
export declare function setSessionId(sessionId: string, authProviderId: string): void;
export declare function getSessionId(): string | null;
/**
 * The id of the auth provider that minted the current session, or, when no
 * session exists, the provider of the last login in this browser (the resume
 * marker). Null in a browser that never logged in or logged out explicitly.
 */
export declare function getLastAuthProviderId(): string | null;
export declare function clearSessionId(): void;
export declare function removeLocalUserData(): void;
/**
 * A ky instance configured for the Wasp API server.
 *
 * Automatically prepends the API base URL, adds authentication headers,
 * and handles session invalidation on 401 responses. Non-2xx responses
 * cause ky to throw an `HTTPError`; pass it through `handleApiError` to
 * get a `WaspHttpError` carrying the server's status code, message, and
 * response body.
 */
export declare const api: import("ky").KyInstance;
/**
 * Exchanges the named auth provider's credential for a Wasp session and
 * stores it, so every subsequent API call is authenticated. The addressed
 * provider rejecting the credential is final -- there is no fallthrough to
 * other providers. Client wiring calls this once after the provider's own
 * login flow succeeds; from then on the provider is off the request path
 * until logout.
 */
export declare function exchangeCredentialForSession(providerId: ExternalAuthProviderId, credential: string): Promise<void>;
/**
 * Takes an error returned by the app's API (as thrown by ky), and transforms it into a more
 * standard format to be further used by the client. It is also assumed that given API
 * error has been formatted as implemented by HttpError on the server.
 */
export declare function handleApiError(error: unknown): unknown;
//# sourceMappingURL=index.d.ts.map