import { HttpError } from './HttpError.js'

// PRIVATE API (server)
/**
 * The one place where the server logs errors it didn't expect.
 * `context` says where the error happened, e.g. `"job sendEmail"`.
 */
export function reportServerError(error: unknown, context: string): void {
  console.error(`Unexpected error in ${context}:`, error)
}

// PRIVATE API (server)
/**
 * Turns anything thrown while handling a request into an `HttpError`
 * that is safe to send to the client.
 *
 * `HttpError`s are thrown on purpose, so they pass through unchanged.
 * Anything else is reported and hidden behind a generic 500, so we
 * don't leak internal details to the client.
 */
export function toHttpError(error: unknown, context: string): HttpError {
  if (error instanceof HttpError) {
    return error
  }
  if (isExposedClientError(error)) {
    return new HttpError(error.status, error.message)
  }
  reportServerError(error, context)
  return new HttpError(500, 'Internal server error')
}

// Express and its body parser create errors with the `http-errors` package.
// It sets `expose: true` on client errors whose message is safe to show,
// e.g. a request with malformed JSON.
function isExposedClientError(
  error: unknown
): error is Error & { status: number } {
  return (
    error instanceof Error &&
    'expose' in error &&
    error.expose === true &&
    'status' in error &&
    typeof error.status === 'number' &&
    error.status >= 400 &&
    error.status < 500
  )
}
