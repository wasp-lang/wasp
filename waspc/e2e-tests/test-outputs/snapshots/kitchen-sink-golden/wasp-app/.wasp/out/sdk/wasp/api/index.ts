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
 * handles session invalidation on 401 responses, and throws
 * ky's `HTTPError` for non-2xx responses.
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
    beforeError: [
      ({ error }) => {
        if (isHTTPError(error)) {
          ;(error as any).statusCode = error.response.status
          const body = error.data as Record<string, unknown> | undefined
          if (body && typeof body.message === 'string') {
            error.message = body.message
          }
        }
        return error
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
  // "Note: This won't work on the same page that is making the changes - it is really a way
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

function getSessionIdFromAuthorizationHeader(header: string | null): string | null {
  const prefix = 'Bearer '
  if (header && header.startsWith(prefix)) {
    return header.substring(prefix.length)
  } else {
    return null
  }
}
