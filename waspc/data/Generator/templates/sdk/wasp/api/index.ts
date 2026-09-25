import ky from 'ky'
import { config } from '../client/config.js'
import { getSessionId, clearSessionId } from '@wasp.sh/lib-sdk-core/browser'
export { getSessionId, setSessionId, clearSessionId, removeLocalUserData, handleApiError } from '@wasp.sh/lib-sdk-core/browser'

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

function getSessionIdFromAuthorizationHeader(header: string | null): string | null {
  const prefix = 'Bearer '
  if (header && header.startsWith(prefix)) {
    return header.substring(prefix.length)
  } else {
    return null
  }
}
