import ky, { isHTTPError } from 'ky'
import type { ExternalAuthProviderId } from '../auth/provider.js'
import { config } from '../client/index.js'
import { storage } from '../core/storage.js'
import { apiEventsEmitter } from './events.js'

const WASP_APP_AUTH_SESSION_ID_NAME = 'sessionId'
// Which provider minted the current session -- and, after the session
// expires, which provider this browser last logged in with. Written on every
// session mint, cleared on explicit logout (so a logout can never be silently
// undone by session resume), deliberately NOT cleared when a session merely
// dies (that is exactly when resume needs it).
const WASP_APP_LAST_AUTH_PROVIDER_ID_NAME = 'lastAuthProviderId'

// PRIVATE API (sdk)
export function setSessionId(sessionId: string, authProviderId: string): void {
  storage.set(WASP_APP_AUTH_SESSION_ID_NAME, sessionId)
  storage.set(WASP_APP_LAST_AUTH_PROVIDER_ID_NAME, authProviderId)
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
/**
 * The id of the auth provider that minted the current session, or, when no
 * session exists, the provider of the last login in this browser (the resume
 * marker). Null in a browser that never logged in or logged out explicitly.
 */
export function getLastAuthProviderId(): string | null {
  const providerId = storage.get(WASP_APP_LAST_AUTH_PROVIDER_ID_NAME) as
    | string
    | undefined
  return providerId ?? null
}

// PRIVATE API (sdk)
// Ends the local session but keeps the last-provider marker: called when the
// session turns out dead (a 401), where silent resume SHOULD get a chance on
// the next auth gate.
export function clearSessionId(): void {
  storage.remove(WASP_APP_AUTH_SESSION_ID_NAME)
  apiEventsEmitter.emit('sessionId.clear')
}

// PRIVATE API (sdk)
// Full teardown, marker included: the explicit-logout path. After this,
// nothing resumes until the next explicit login.
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
      // Every request authenticates with Wasp's own session and nothing else:
      // there is deliberately no provider machinery on the request path.
      // Sessionless requests go out unauthenticated (and 401 on protected
      // operations); silent session resume happens only at the auth gate
      // (`createAuthRequiredPage`), addressed to the provider of the last
      // login.
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

/**
 * Exchanges an auth provider's credential for a Wasp session
 * (`POST /auth/login/:providerId`). Uses plain `fetch` rather than the `api`
 * instance so the request does not recurse through the hooks above. The
 * provider id rides in the path percent-encoded, in one place, because ids
 * contain a ':' ('external:clerk').
 */
async function fetchSessionForCredential(
  providerId: ExternalAuthProviderId,
  credential: string,
): Promise<string | null> {
  const response = await fetch(buildExchangeUrl(providerId), {
    method: 'POST',
    headers: { Authorization: `Bearer ${credential}` },
  })
  if (!response.ok) {
    return null
  }
  const { sessionId } = (await response.json()) as { sessionId: string }
  return sessionId
}

function buildExchangeUrl(providerId: ExternalAuthProviderId): string {
  return `${config.apiUrl}/auth/login/${encodeURIComponent(providerId)}`
}

// PUBLIC API
/**
 * Exchanges the named auth provider's credential for a Wasp session and
 * stores it, so every subsequent API call is authenticated. The addressed
 * provider rejecting the credential is final -- there is no fallthrough to
 * other providers. Client wiring calls this once after the provider's own
 * login flow succeeds; from then on the provider is off the request path
 * until logout.
 */
export async function exchangeCredentialForSession(
  providerId: ExternalAuthProviderId,
  credential: string,
): Promise<void> {
  const sessionId = await fetchSessionForCredential(providerId, credential)
  if (sessionId === null) {
    throw new Error(`Exchanging the '${providerId}' auth provider credential for a session failed.`)
  }
  setSessionId(sessionId, providerId)
}

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
