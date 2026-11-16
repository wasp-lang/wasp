/**
 * This module is concerned with errors we don't recognize.
 *
 * Like Express's default error handler, we respect their status code and
 * headers when they carry a valid status code. We can't know whether their
 * message is safe to show, so we never send it.
 *
 * @see https://expressjs.com/en/guide/error-handling.html#the-default-error-handler
 */

import * as Wasp from "@wasp/lib-sdk-core";
import { STATUS_CODES } from "node:http";
import { getErrorDetails } from "./utils.js";

// PRIVATE API (server)
/**
 * Turns an unkown error into a `Wasp.ServerHttpError`.
 *
 * - It keeps the status code and headers when there is a valid status code,
 *   otherwise defaults to a generic 500 error.
 * - It always hides the message behind the status code's generic one.
 */
export function toWaspHttpError(
  error: unknown,
  { includeErrorDetails }: { includeErrorDetails: boolean },
): Wasp.ServerHttpError {
  const data = includeErrorDetails ? getErrorDetails(error) : undefined;
  const statusCode = findStatusCode(error);
  if (statusCode === undefined) {
    return new Wasp.ServerHttpError(500, STATUS_CODES[500], data);
  }
  return new Wasp.ServerHttpError(statusCode, STATUS_CODES[statusCode], data, {
    headers: findHeaders(error),
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
  return [status, statusCode].find(Wasp.isHttpErrorStatusCode);
}


function findHeaders(error: unknown): Wasp.ServerHttpErrorHeaders | undefined {
  if (
    typeof error === "object" &&
    error !== null &&
    "headers" in error &&
    typeof error.headers === "object" &&
    error.headers !== null
  ) {
    return { ...error.headers } as Wasp.ServerHttpErrorHeaders;
  }
  return undefined;
}
