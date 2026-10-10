import type { Response } from "express";
import { STATUS_CODES } from "node:http";
import { HttpError, toHttpErrorBody } from "../errors/HttpError.js";

// PRIVATE API (server)
/**
 * Where an unexpected server error happened.
 *
 * It's structured data instead of a plain string, like the bindings of a
 * pino child logger or Sentry's tags, so we can later group errors by
 * `source` (e.g. show all job errors in a dashboard).
 */
export type ServerErrorContext =
  | { source: "request" }
  | { source: "job"; jobName: string }
  | { source: "webSocket" }
  | { source: "oauth" }
  | { source: "startup" }
  | { source: "dbSeed" };

// PRIVATE API (server)
/**
 * The one place where the server logs errors it didn't expect.
 */
export function reportServerError(
  error: unknown,
  context: ServerErrorContext,
): void {
  console.error(`Unexpected error in ${formatContext(context)}:`, error);
}

// PRIVATE API (server)
/**
 * Sends anything thrown while handling a request as a JSON error response.
 *
 * - An `HttpError` is thrown on purpose, so we send its status code,
 *   message, and data.
 * - Errors from Express and other middleware keep their status code and
 *   headers, the same as with Express's default error handler. We only send
 *   their message when they mark it as safe to show.
 * - We report everything else and hide it behind a generic 500, so we don't
 *   leak internal details to the client.
 */
export function sendErrorResponse(response: Response, error: unknown): void {
  const httpError = toHttpError(error);
  const headers = getMiddlewareErrorHeaders(error);
  if (headers) {
    response.set(headers);
  }
  response.status(httpError.statusCode).json(toHttpErrorBody(httpError));
}

function toHttpError(error: unknown): HttpError {
  if (error instanceof HttpError) {
    return error;
  }

  const statusCode = getMiddlewareErrorStatusCode(error);
  if (statusCode === undefined || statusCode >= 500) {
    reportServerError(error, { source: "request" });
  }
  if (statusCode === undefined) {
    return new HttpError(500, STATUS_CODES[500]);
  }

  const message = isMiddlewareErrorExposed(error)
    ? error.message
    : STATUS_CODES[statusCode];
  return new HttpError(statusCode, message);
}

/**
 * Express and its middleware create errors with the `http-errors` package,
 * which puts the response status code in `status` (or `statusCode`).
 *
 * @see https://github.com/jshttp/http-errors#error-properties
 * @see https://expressjs.com/en/guide/error-handling.html#the-default-error-handler
 */
function getMiddlewareErrorStatusCode(error: unknown): number | undefined {
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

/**
 * `http-errors` sets `expose: true` when the message is safe to show to the
 * client, which is the default for 4xx errors.
 *
 * @see https://github.com/jshttp/http-errors#error-properties
 */
function isMiddlewareErrorExposed(error: unknown): error is Error {
  return error instanceof Error && "expose" in error && error.expose === true;
}

/**
 * `http-errors` lets an error carry response headers, e.g. `Retry-After`
 * on a 429.
 *
 * @see https://github.com/jshttp/http-errors#error-properties
 */
function getMiddlewareErrorHeaders(
  error: unknown,
): Record<string, string> | undefined {
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

function formatContext({ source, ...details }: ServerErrorContext): string {
  return [source, ...Object.values(details)].join(" ");
}
