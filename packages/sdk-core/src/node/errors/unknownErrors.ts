/**
 * This module is concerned with errors we don't recognize.
 *
 * Like Express's default error handler, we respect their status code and
 * headers when they carry a valid status code. We can't know whether their
 * message is safe to show, so we never send it.
 *
 * @see https://expressjs.com/en/guide/error-handling.html#the-default-error-handler
 */

import { STATUS_CODES } from "node:http";
import { HttpError, isHttpErrorStatusCode } from "../../HttpError.js";
import {
  getErrorDetails,
  toHeadersInit,
  type UnknownErrorHeaders,
} from "./utils.js";

/**
 * Turns an unkown error into a `HttpError`.
 *
 * - It keeps the status code and headers when there is a valid status code,
 *   otherwise defaults to a generic 500 error.
 * - It always hides the message behind the status code's generic one.
 */
export function toHttpError(
  error: unknown,
  { includeErrorDetails }: { includeErrorDetails: boolean },
): HttpError {
  const data = includeErrorDetails ? getErrorDetails(error) : undefined;
  const statusCode = findStatusCode(error);
  if (statusCode === undefined) {
    return new HttpError(500, STATUS_CODES[500], { data });
  }
  return new HttpError(statusCode, STATUS_CODES[statusCode], {
    data,
    headers: toHeadersInit(findHeaders(error)),
  });
}

function findStatusCode(error: unknown): number | undefined {
  if (typeof error !== "object" || error === null) {
    return undefined;
  }
  const { status, statusCode } = error as {
    status?: unknown;
    statusCode?: unknown;
  };
  return [status, statusCode].find(isHttpErrorStatusCode);
}

function findHeaders(error: unknown): UnknownErrorHeaders | undefined {
  if (
    typeof error === "object" &&
    error !== null &&
    "headers" in error &&
    typeof error.headers === "object" &&
    error.headers !== null
  ) {
    // Like Express, we copy the object's own properties. A `Headers` instance
    // has none, so it gives no headers.
    return { ...error.headers };
  }
  return undefined;
}
