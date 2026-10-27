import { HttpError } from "@wasp.sh/lib-sdk-core";
import type { Response } from "express";
import config from "../config.js";
import * as httpErrors from "./httpErrors.js";
import * as unknownErrors from "./unknownErrors.js";

// PRIVATE API (server)
// TODO: Refactor this once Wasp has proper logging.
export type ServerErrorContext =
  | { source: "request" }
  | { source: "job"; jobName: string }
  | { source: "webSocket" }
  | { source: "oauth" }
  | { source: "startup" }
  | { source: "dbSeed" };

// PRIVATE API (server)
// TODO: Refactor this once Wasp has proper logging.
export function reportUnexpectedServerError(
  error: unknown,
  context: ServerErrorContext,
): void {
  console.error(`Unexpected error in ${formatContext(context)}:`, error);
}

// PRIVATE API (server)
/**
 * Sends anything thrown while handling a request as a JSON error response.
 *
 * - Wasp's `HttpError`s are thrown on purpose, so we return them as is.
 * - `http-errors` package errors, which are thrown by `express`, are
 *   properly transformed into `HttpError`s.
 * - For the rest of errors, we try to make a choice based on their fields.
 *   If we manage to find a valid status code, we return a best bet `HttpError`,
 *   otherwise we default to a generic 500 error.
 *
 * Every error, that wasn't a `HttpError` originally, logs if it's an 5xx error.
 *
 * In development, we purposely attach otherwise hidden data (message, stack traces)
 * where possible.
 */
export function sendErrorResponse(response: Response, error: unknown): void {
  const httpError = toHttpError(error);
  if (httpError.headers) {
    response.set(httpError.headers);
  }
  response
    .status(httpError.statusCode)
    .json({ message: httpError.message, data: httpError.data });
}

function toHttpError(error: unknown): HttpError {
  if (error instanceof HttpError) {
    return error;
  }

  const options = { includeErrorDetails: config.isDevelopment };

  // `express` uses `http-errors` package to create errors
  const httpError = httpErrors.isHttpErrorsError(error)
    ? httpErrors.toWaspHttpError(error, options)
    : unknownErrors.toWaspHttpError(error, options);

  if (httpError.statusCode >= 500) {
    reportUnexpectedServerError(error, { source: "request" });
  }
  return httpError;
}

function formatContext({ source, ...details }: ServerErrorContext): string {
  return [source, ...Object.values(details)].join(" ");
}
