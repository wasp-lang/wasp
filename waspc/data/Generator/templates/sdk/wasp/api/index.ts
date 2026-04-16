import ky, { isHTTPError } from 'ky'
import { config } from 'wasp/client'
import { storage } from 'wasp/core/storage'
import { apiEventsEmitter } from './events.js'

const WASP_APP_AUTH_SESSION_ID_NAME = 'sessionId'

// PRIVATE API (sdk)
export function setSessionId(sessionId: string): void {
  storage.set(WASP_APP_AUTH_SESSION_ID_NAME, sessionId)
  apiEventsEmitter.emit('sessionId.set')
}

// PRIVATE API (sdk)
export function getSessionId(): string | null {
  const sessionId = storage.get(WASP_APP_AUTH_SESSION_ID_NAME) as
    | string
    | undefined
  return sessionId ?? null
}
// PRIVATE API (sdk)
export function clearSessionId(): void {
  storage.remove(WASP_APP_AUTH_SESSION_ID_NAME)
  apiEventsEmitter.emit('sessionId.clear')
}

// PRIVATE API (sdk)
export function removeLocalUserData(): void {
  storage.clear()
  apiEventsEmitter.emit('sessionId.clear')
}

// PUBLIC API
/**
 * A ky instance configured for the Wasp API server.
 *
 * Automatically prepends the API base URL, adds authentication headers,
 * and handles session invalidation on 401 responses. Non-2xx responses
 * cause ky to throw an `HTTPError`; pass it through `handleApiError` to
 * get a `WaspHttpError` carrying the server's status code, message, and
 * response body.
 */
export const api = ky.extend({
  prefix: config.apiUrl,
  hooks: {
    beforeRequest: [
      ({ request }) => {
        const sessionId = getSessionId()
        if (sessionId !== null) {
          request.headers.set('Authorization', `Bearer ${sessionId}`)
        }
      },
    ],
    afterResponse: [
      ({ request, response }) => {
        if (response.status === 401) {
          // Before clearing the session ID from local storage due to a 401 error,
          // compare the session ID stored in the *failed request's* headers
          // with the *current* session ID in local storage.
          // Only clear the local session ID if the two session IDs match.
          //
          // This prevents a race condition like this:
          // 1. Request A is sent with old session ID X.
          // 2. User logs out and logs back in, obtaining new session ID Y.
          // 3. Request A finally fails with a 401 (because ID X is invalid).
          // Without the check, we would clear the *current* valid session ID Y.
          // The check ensures we only clear the session if the *request that failed*
          // used the *same session ID that's currently stored*.
          const failingSessionId = getSessionIdFromAuthorizationHeader(
            request.headers.get('Authorization')
          )
          const currentSessionId = getSessionId()
          if (failingSessionId === currentSessionId) {
            clearSessionId()
          }
        }
      },
    ],
  },
})

// This makes sure that the following handler won't try to run in a non-browser
// environment (e.g. during SSR), where `window` is not defined.
if (typeof window !== 'undefined') {
  // This handler will run on other tabs (not the active one calling API functions),
  // and will ensure they know about auth session ID changes.
  // Ref: https://developer.mozilla.org/en-US/docs/Web/API/Window/storage_event
  // "Note: This won't work on the same page that is making the changes — it is really a way
  // for other pages on the domain using the storage to sync any changes that are made."
  window.addEventListener('storage', (event) => {
    if (event.key === storage.getPrefixedKey(WASP_APP_AUTH_SESSION_ID_NAME)) {
      if (!!event.newValue) {
        apiEventsEmitter.emit('sessionId.set')
      } else {
        apiEventsEmitter.emit('sessionId.clear')
      }
    }
  })
}

// PRIVATE API (sdk)
/**
 * Takes an error returned by the app's API (as thrown by ky), and transforms it into a more
 * standard format to be further used by the client. It is also assumed that given API
 * error has been formatted as implemented by HttpError on the server.
 */
export function handleApiError(error: unknown): unknown {
  if (isHTTPError(error)) {
    // If error came from HTTP response, we capture most informative message
    // and also add .statusCode information to it.
    // If error had JSON response, we assume it is of format { message, data } and
    // add that info to the error.
    // TODO: We might want to use HttpError here instead of just Error, since
    //   HttpError is also used on server to throw errors like these.
    //   That would require copying HttpError code to web-app also and using it here.
    const responseJson = error.data as { message?: string; data?: unknown } | undefined
    const responseStatusCode = error.response.status
    return new WaspHttpError(responseStatusCode, responseJson?.message ?? error.message, responseJson)
  } else {
    // If any other error, we just propagate it.
    return error
  }
}

class WaspHttpError extends Error {
  statusCode: number

  data: unknown

  constructor(statusCode: number, message: string, data: unknown) {
    super(message)
    this.statusCode = statusCode
    this.data = data
  }
}

function getSessionIdFromAuthorizationHeader(header: string | null): string | null {
  const prefix = 'Bearer '
  if (header && header.startsWith(prefix)) {
    return header.substring(prefix.length)
  } else {
    return null
  }
}
