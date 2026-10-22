/**
 * This module is concerned with handling `http-errors` package errors specifically,
 * not HTTP errors in general.
 *
 * `http-errors` is what `express` uses.
 *
 * @see https://github.com/jshttp/http-errors#error-properties
 * @see https://expressjs.com/en/guide/error-handling.html#the-default-error-handler
 */

import * as Wasp from "@wasp.sh/lib-sdk-core";
import { STATUS_CODES } from "node:http";

// PRIVATE API (server)
/**
 * Turns an error thrown by Express or other Express middleware into a
 * `Wasp.HttpError`.
 *
 * - It keeps the status code, or uses 500 when there isn't one.
 * - It only keeps the message when `expose` marks it as safe to show.
 * - With `includeErrorDetails`, a hidden error's message and stack go into
 *   `data`, like Express's default error handler shows them on its HTML page.
 */
export function toWaspHttpError(
  error: unknown,
  { includeErrorDetails }: { includeErrorDetails: boolean },
): Wasp.HttpError {
  const statusCode = getStatusCode(error) ?? 500;
  if (isMessageExposed(error)) {
    return new Wasp.HttpError(statusCode, error.message);
  }
  return new Wasp.HttpError(
    statusCode,
    STATUS_CODES[statusCode],
    includeErrorDetails ? getErrorDetails(error) : undefined,
  );
}

/**
 * The response status code, from `status` (or `statusCode`).
 */
function getStatusCode(error: unknown): number | undefined {
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
 * Whether the message is safe to show to the client, from `expose`.
 * `http-errors` sets it by default for 4xx errors.
 */
function isMessageExposed(error: unknown): error is Error {
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

function getErrorDetails(error: unknown): {
  message: string;
  stack: string | undefined;
} {
  return {
    message: Wasp.getErrorMessage(error),
    stack: error instanceof Error ? error.stack : undefined,
  };
}
