import ky, { isHTTPError } from 'ky'
import { config } from '../client/index.js'
import { storage } from '../core/storage.js'
import { apiEventsEmitter } from './events.js'

// The stored key is still called "sessionId" so an existing browser session
// survives an upgrade.
const WASP_APP_AUTH_CREDENTIAL_NAME = 'sessionId'
// Which scheme the current credential came from, so logout can tell that
// scheme's client auth handler to clear its own state.
const WASP_APP_LAST_AUTH_SCHEME_NAME = 'lastAuthScheme'

// PRIVATE API (sdk)
/**
 * Adopt a bearer credential a scheme obtained through its own routes, or drop
 * it with `null`. Cookie-carried credentials never go through here.
 */
export function setCredential(
  credential: string | null,
  scheme: string,
  options?: { persistent?: boolean },
): void {
  removeStoredCredential()
  if (credential === null) {
    apiEventsEmitter.emit('sessionId.clear')
    return
  }
  // A sign-in without "remember me" lives in sessionStorage: this tab only,
  // gone when the browser session ends.
  if (options?.persistent === false && browserSessionStorage !== null) {
    browserSessionStorage.setItem(SESSION_ONLY_CREDENTIAL_KEY, credential)
  } else {
    storage.set(WASP_APP_AUTH_CREDENTIAL_NAME, credential)
  }
  storage.set(WASP_APP_LAST_AUTH_SCHEME_NAME, scheme)
  apiEventsEmitter.emit('sessionId.set')
}

const SESSION_ONLY_CREDENTIAL_KEY = storage.getPrefixedKey(WASP_APP_AUTH_CREDENTIAL_NAME)
const browserSessionStorage: Storage | null =
  typeof window === 'undefined' || !window.sessionStorage ? null : window.sessionStorage

function removeStoredCredential(): void {
  storage.remove(WASP_APP_AUTH_CREDENTIAL_NAME)
  browserSessionStorage?.removeItem(SESSION_ONLY_CREDENTIAL_KEY)
}

// PRIVATE API (sdk)
export function getCredential(): string | null {
  const sessionOnlyCredential = browserSessionStorage?.getItem(SESSION_ONLY_CREDENTIAL_KEY) ?? null
  if (sessionOnlyCredential !== null) {
    return sessionOnlyCredential
  }
  const credential = storage.get(WASP_APP_AUTH_CREDENTIAL_NAME) as
    | string
    | undefined
  return credential ?? null
}

// PRIVATE API (sdk)
/**
 * The scheme that issued the stored credential, or, when none is stored, the
 * scheme of the last login in this browser. Null in a browser that never
 * logged in or logged out explicitly.
 */
export function getLastAuthScheme(): string | null {
  const scheme = storage.get(WASP_APP_LAST_AUTH_SCHEME_NAME) as
    | string
    | undefined
  return scheme ?? null
}

// PRIVATE API (sdk)
// Drops a credential the server rejected (a 401).
export function clearCredential(): void {
  removeStoredCredential()
  apiEventsEmitter.emit('sessionId.clear')
}

// PRIVATE API (sdk)
// Full teardown, marker included: the explicit-logout path.
export function removeLocalUserData(): void {
  removeStoredCredential()
  storage.clear()
  apiEventsEmitter.emit('sessionId.clear')
}

// The fallback credential source for requests: the default scheme's client
// handler, registered by the scheme registry. Consulted only when no
// Wasp-issued bearer credential is stored.
let credentialSource: (() => Promise<string | null>) | null = null

// PRIVATE API (sdk)
export function registerCredentialSource(source: () => Promise<string | null>): void {
  credentialSource = source
}

// PRIVATE API (sdk)
/** The credential the next request should carry, if any. */
export async function getRequestCredential(): Promise<string | null> {
  const stored = getCredential()
  if (stored !== null) {
    return stored
  }
  return credentialSource === null ? null : credentialSource()
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
      // Bearer credentials ride in the Authorization header: a Wasp-issued
      // one from local storage, else the default scheme's own (a hosted
      // provider's token). Cookie credentials are the browser's business.
      async ({ request }) => {
        const credential = await getRequestCredential()
        if (credential !== null) {
          request.headers.set('Authorization', `Bearer ${credential}`)
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
          const failingCredential = getCredentialFromAuthorizationHeader(
            request.headers.get('Authorization')
          )
          const currentCredential = getCredential()
          if (failingCredential !== null && failingCredential === currentCredential) {
            clearCredential()
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
    if (event.key === storage.getPrefixedKey(WASP_APP_AUTH_CREDENTIAL_NAME)) {
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

function getCredentialFromAuthorizationHeader(header: string | null): string | null {
  const prefix = 'Bearer '
  if (header && header.startsWith(prefix)) {
    return header.substring(prefix.length)
  } else {
    return null
  }
}
