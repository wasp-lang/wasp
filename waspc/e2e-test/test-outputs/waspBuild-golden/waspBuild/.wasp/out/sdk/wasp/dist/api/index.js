import axios from 'axios';
import config from 'wasp/core/config';
import { storage } from 'wasp/core/storage';
import { apiEventsEmitter } from './events.js';
// PUBLIC API
export const api = axios.create({
    baseURL: config.apiUrl,
});
const WASP_APP_AUTH_SESSION_ID_NAME = 'sessionId';
let waspAppAuthSessionId = storage.get(WASP_APP_AUTH_SESSION_ID_NAME);
// PRIVATE API (sdk)
export function setSessionId(sessionId) {
    waspAppAuthSessionId = sessionId;
    storage.set(WASP_APP_AUTH_SESSION_ID_NAME, sessionId);
    apiEventsEmitter.emit('sessionId.set');
}
// PRIVATE API (sdk)
export function getSessionId() {
    return waspAppAuthSessionId;
}
// PRIVATE API (sdk)
export function clearSessionId() {
    waspAppAuthSessionId = undefined;
    storage.remove(WASP_APP_AUTH_SESSION_ID_NAME);
    apiEventsEmitter.emit('sessionId.clear');
}
// PRIVATE API (sdk)
export function removeLocalUserData() {
    waspAppAuthSessionId = undefined;
    storage.clear();
    apiEventsEmitter.emit('sessionId.clear');
}
api.interceptors.request.use((request) => {
    const sessionId = getSessionId();
    if (sessionId) {
        request.headers['Authorization'] = `Bearer ${sessionId}`;
    }
    return request;
});
api.interceptors.response.use(undefined, (error) => {
    var _a;
    if (((_a = error.response) === null || _a === void 0 ? void 0 : _a.status) === 401) {
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
            waspAppAuthSessionId = event.newValue;
            apiEventsEmitter.emit('sessionId.set');
        }
        else {
            waspAppAuthSessionId = undefined;
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
    var _a, _b;
    if (error === null || error === void 0 ? void 0 : error.response) {
        // If error came from HTTP response, we capture most informative message
        // and also add .statusCode information to it.
        // If error had JSON response, we assume it is of format { message, data } and
        // add that info to the error.
        // TODO: We might want to use HttpError here instead of just Error, since
        //   HttpError is also used on server to throw errors like these.
        //   That would require copying HttpError code to web-app also and using it here.
        const responseJson = (_a = error.response) === null || _a === void 0 ? void 0 : _a.data;
        const responseStatusCode = error.response.status;
        throw new WaspHttpError(responseStatusCode, (_b = responseJson === null || responseJson === void 0 ? void 0 : responseJson.message) !== null && _b !== void 0 ? _b : error.message, responseJson);
    }
    else {
        // If any other error, we just propagate it.
        throw error;
    }
}
class WaspHttpError extends Error {
    constructor(statusCode, message, data) {
        super(message);
        this.statusCode = statusCode;
        this.data = data;
    }
}
//# sourceMappingURL=index.js.map