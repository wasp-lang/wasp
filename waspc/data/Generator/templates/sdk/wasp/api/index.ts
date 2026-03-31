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
 * Makes an HTTP request to the Wasp API server.
 *
 * Takes the same parameters as the `Request` constructor (input, init?),
 * prepends the API base URL for relative paths, adds authentication headers,
 * and handles session invalidation on 401 responses.
 *
 * Throws `WaspHttpError` for non-2xx responses.
 */
export async function api(input: RequestInfo | URL, init?: RequestInit): Promise<Response> {
  const request = new Request(toAbsoluteUrl(input), init)

  const sessionId = getSessionId()
  if (sessionId !== null) {
    request.headers.set('Authorization', `Bearer ${sessionId}`)
  }

  const response = await fetch(request)

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

  if (!response.ok) {
    let message = response.statusText
    let data: unknown
    try {
      const responseJson = await response.json()
      message = responseJson?.message ?? message
      data = responseJson
    } catch {
      // Response body is not JSON, use statusText as message
    }
    throw new WaspHttpError(response.status, message, data)
  }

  return response
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
export class WaspHttpError extends Error {
  statusCode: number

  data: unknown

  constructor (statusCode: number, message: string, data: unknown) {
    super(message)
    this.statusCode = statusCode
    this.data = data
  }
}

function getSessionIdFromAuthorizationHeader(header: string | null | undefined): string | null {
  const prefix = 'Bearer '
  if (header && header.startsWith(prefix)) {
    return header.substring(prefix.length)
  } else {
    return null
  }
}

function toAbsoluteUrl(input: RequestInfo | URL): RequestInfo | URL {
  if (typeof input === 'string' && input.startsWith('/')) {
    return config.apiUrl + input
  }
  if (input instanceof URL) {
    return input
  }
  if (input instanceof Request) {
    if (input.url.startsWith('/')) {
      return new Request(config.apiUrl + input.url, input)
    }
  }
  return input
}
