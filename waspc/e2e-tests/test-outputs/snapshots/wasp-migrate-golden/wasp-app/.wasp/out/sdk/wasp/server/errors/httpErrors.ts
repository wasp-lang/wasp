/**
 * This module is concerned with handling `http-errors` package errors specifically,
 * not HTTP errors in general.
 *
 * `http-errors` is what `express` uses.
 *
 * @see https://github.com/jshttp/http-errors#error-properties
 * @see https://expressjs.com/en/guide/error-handling.html#the-default-error-handler
 */

// PRIVATE API (server)
/**
 * The response status code, from `status` (or `statusCode`).
 */
export function getStatusCode(error: unknown): number | undefined {
  if (typeof error !== "object" || error === null) {
    return undefined;
  }
  for (const key of ["status", "statusCode"]) {
    const statusCode = (error as Record<string, unknown>)[key];
    if (isErrorStatusCode(statusCode)) {
      return statusCode;
    }
  }
  return undefined;
}

// PRIVATE API (server)
/**
 * Whether the message is safe to show to the client, from `expose`.
 * `http-errors` sets it by default for 4xx errors.
 */
export function isMessageExposed(error: unknown): error is Error {
  return error instanceof Error && "expose" in error && error.expose === true;
}

// PRIVATE API (server)
/**
 * The response headers, from `headers`, e.g. `Retry-After` on a 429.
 */
export function getHeaders(error: unknown): Record<string, string> | undefined {
  if (
    typeof error === "object" &&
    error !== null &&
    "headers" in error &&
    typeof error.headers === "object" &&
    error.headers !== null
  ) {
    return error.headers as Record<string, string>;
  }
  return undefined;
}

function isErrorStatusCode(value: unknown): value is number {
  return (
    typeof value === "number" &&
    Number.isInteger(value) &&
    value >= 400 &&
    value < 600
  );
}
