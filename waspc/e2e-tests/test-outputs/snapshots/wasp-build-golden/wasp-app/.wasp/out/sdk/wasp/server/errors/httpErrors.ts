/**
 * This module is concerned with handling `http-errors` package errors specifically,
 * not HTTP errors in general.
 *
 * `http-errors` is what `express` uses.
 *
 * @see https://github.com/jshttp/http-errors#error-properties
 */

import * as Wasp from "@wasp.sh/lib-sdk-core";
import { STATUS_CODES } from "node:http";
import { getErrorDetails, isErrorStatusCode } from "./utils.js";

// PRIVATE API (server)
export interface HttpErrorsError extends Error {
  status: number;
  statusCode: number;
  expose: boolean;
  headers?: Wasp.HttpErrorHeaders;
}

// PRIVATE API (server)
/**
 * The same check as `http-errors`'s own `isHttpError`, plus a check that the
 * status code is an error one, which `http-errors` doesn't guarantee.
 *
 * @see https://github.com/jshttp/http-errors/blob/v2.0.1/index.js#L176-L188
 */
export function isHttpErrorsError(error: unknown): error is HttpErrorsError {
  return (
    error instanceof Error &&
    "expose" in error &&
    typeof error.expose === "boolean" &&
    "statusCode" in error &&
    isErrorStatusCode(error.statusCode) &&
    "status" in error &&
    error.status === error.statusCode
  );
}

// PRIVATE API (server)
/**
 * Turns an error thrown by Express or other Express middleware into a
 * `Wasp.HttpError`.
 *
 * - It keeps the status code and headers, e.g. `Retry-After` on a 429.
 * - It only keeps the message when `expose` marks it as safe to show.
 * - With `includeErrorDetails`, a hidden error's message and stack go into
 *   `data`, like Express's default error handler shows them on its HTML page.
 */
export function toWaspHttpError(
  error: HttpErrorsError,
  { includeErrorDetails }: { includeErrorDetails: boolean },
): Wasp.HttpError {
  const options = { headers: error.headers };
  if (error.expose) {
    return new Wasp.HttpError(
      error.statusCode,
      error.message,
      undefined,
      options,
    );
  }
  return new Wasp.HttpError(
    error.statusCode,
    STATUS_CODES[error.statusCode],
    includeErrorDetails ? getErrorDetails(error) : undefined,
    options,
  );
}
