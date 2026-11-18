import { HttpError, toHttpErrorBody } from "@wasp/lib-sdk-core";
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
 * 1. An `HttpError` is thrown on purpose, so we send it as is.
 * 2. Anything else is unexpected:
 *    - `http-errors` package errors, which are thrown by `express`, are
 *      properly transformed into `HttpError`s.
 *    - For the rest of errors, we try to make a choice based on their fields.
 *      If we manage to find a valid status code, we return a best bet
 *      `HttpError`, otherwise we default to a generic 500 error.
 *
 * Every unexpected error logs if it's an 5xx error.
 *
 * In development, we purposely attach otherwise hidden data (message, stack traces)
 * where possible.
 */
export function sendErrorResponse(response: Response, error: unknown): void {
  const httpError = error instanceof HttpError ? error : toHttpError(error);

  for (const [name, value] of httpError.response.headers) {
    // Express sets the content type itself when it sends the body.
    if (name !== "content-type") {
      response.append(name, value);
    }
  }
  response.status(httpError.status).json(toHttpErrorBody(httpError));
}

function toHttpError(unexpectedError: unknown): HttpError {
  const options = { includeErrorDetails: config.isDevelopment };

  // `express` uses `http-errors` package to create errors
  const httpError = httpErrors.isHttpErrorsError(unexpectedError)
    ? httpErrors.toHttpError(unexpectedError, options)
    : unknownErrors.toHttpError(unexpectedError, options);

  if (httpError.status >= 500) {
    reportUnexpectedServerError(unexpectedError, { source: "request" });
  }
  return httpError;
}

function formatContext({ source, ...details }: ServerErrorContext): string {
  return [source, ...Object.values(details)].join(" ");
}
