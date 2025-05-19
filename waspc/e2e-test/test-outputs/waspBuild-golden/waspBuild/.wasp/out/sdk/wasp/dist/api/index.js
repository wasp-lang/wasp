import axios from 'axios';
import { config } from 'wasp/client';
import { storage } from 'wasp/core/storage';
import { apiEventsEmitter } from './events.js';
// PUBLIC API
export const api = axios.create({
    baseURL: config.apiUrl,
});
const WASP_APP_AUTH_SESSION_ID_NAME = 'sessionId';
// PRIVATE API (sdk)
export function setSessionId(sessionId) {
    storage.set(WASP_APP_AUTH_SESSION_ID_NAME, sessionId);
    apiEventsEmitter.emit('sessionId.set');
}
// PRIVATE API (sdk)
export function getSessionId() {
    const sessionId = storage.get(WASP_APP_AUTH_SESSION_ID_NAME);
    return sessionId ?? null;
}
// PRIVATE API (sdk)
export function clearSessionId() {
    storage.remove(WASP_APP_AUTH_SESSION_ID_NAME);
    apiEventsEmitter.emit('sessionId.clear');
}
// PRIVATE API (sdk)
export function removeLocalUserData() {
    storage.clear();
    apiEventsEmitter.emit('sessionId.clear');
}
/**
 * Axios interceptors for handling authentication
 *
 * (1) Request Interceptor:
 * If a session ID exists, it is added to the request as an `Authorization`
 * header for the server to use.
 *
 * (2) Response Interceptor:
 * - Catches 401 errors from the server.
 * - Before clearing the session ID from local storage due to a 401 error,
 *   it compares the session ID stored in the *failed request's config*
 *   with the *current* session ID in local storage.
 * - It only clears the local session ID if the two session IDs match.
 *
 * This prevents a race condition like this:
 * 1. Request A is sent with old session ID X.
 * 2. User logs out and logs back in, obtaining new session ID Y.
 * 3. Request A finally fails with a 401 (because ID X is invalid).
 * Without the check, the interceptor would clear the *current* valid session ID Y.
 * The check ensures we only clear the session if the *request that failed* used
 * the *same session ID that's currently stored*.
 */
api.interceptors.request.use((config) => {
    const sessionId = getSessionId();
    if (sessionId !== null) {
        config.headers['Authorization'] = `Bearer ${sessionId}`;
    }
    return config;
});
api.interceptors.response.use(undefined, (error) => {
    const failingSessionId = getSessionIdFromAuthorizationHeader(error.config.headers['Authorization']);
    const currentSessionId = getSessionId();
    if (error.response?.status === 401 && failingSessionId === currentSessionId) {
        clearSessionId();
    }
    return Promise.reject(error);
});
// This handler will run on other tabs (not the active one calling API functions),
// and will ensure they know about auth session ID changes.
// Ref: https://developer.mozilla.org/en-US/docs/Web/API/Window/storage_event
// "Note: This won't work on the same page that is making the changes — it is really a way
// for other pages on the domain using the storage to sync any changes that are made."
window.addEventListener('storage', (event) => {
    if (event.key === storage.getPrefixedKey(WASP_APP_AUTH_SESSION_ID_NAME)) {
        if (!!event.newValue) {
            apiEventsEmitter.emit('sessionId.set');
        }
        else {
            apiEventsEmitter.emit('sessionId.clear');
        }
    }
});
// PRIVATE API (sdk)
/**
 * Takes an error returned by the app's API (as returned by axios), and transforms into a more
 * standard format to be further used by the client. It is also assumed that given API
 * error has been formatted as implemented by HttpError on the server.
 */
export function handleApiError(error) {
    if (error?.response) {
        // If error came from HTTP response, we capture most informative message
        // and also add .statusCode information to it.
        // If error had JSON response, we assume it is of format { message, data } and
        // add that info to the error.
        // TODO: We might want to use HttpError here instead of just Error, since
        //   HttpError is also used on server to throw errors like these.
        //   That would require copying HttpError code to web-app also and using it here.
        const responseJson = error.response?.data;
        const responseStatusCode = error.response.status;
        return new WaspHttpError(responseStatusCode, responseJson?.message ?? error.message, responseJson);
    }
    else {
        // If any other error, we just propagate it.
        return error;
    }
}
class WaspHttpError extends Error {
    statusCode;
    data;
    constructor(statusCode, message, data) {
        super(message);
        this.statusCode = statusCode;
        this.data = data;
    }
}
function getSessionIdFromAuthorizationHeader(header) {
    const prefix = 'Bearer ';
    if (header && header.startsWith(prefix)) {
        return header.substring(prefix.length);
    }
    else {
        return null;
    }
}
//# sourceMappingURL=index.js.map