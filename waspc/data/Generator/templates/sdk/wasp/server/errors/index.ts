import {
  HttpError,
  ServerHttpError,
  toHttpErrorBody,
  type ServerHttpErrorHeaders,
} from "@wasp/lib-sdk-core";
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
 * We handle the error as the most specific kind we recognize:
 *
 * 1. `ServerHttpError` and `HttpError` are thrown on purpose, so we send them
 *    as is. Only `ServerHttpError` includes headers.
 * 2. Anything else is unexpected:
 *    - `http-errors` package errors, which are thrown by `express`, are
 *      properly transformed into `ServerHttpError`s.
 *    - For the rest of errors, we try to make a choice based on their fields.
 *      If we manage to find a valid status code, we return a best bet
 *      `ServerHttpError`, otherwise we default to a generic 500 error.
 *
 * Every unexpected error logs if it's an 5xx error.
 *
 * In development, we purposely attach otherwise hidden data (message, stack traces)
 * where possible.
 */
export function sendErrorResponse(response: Response, error: unknown): void {
  if (error instanceof ServerHttpError) {
    sendHttpError(response, error, error.headers);
  } else if (error instanceof HttpError) {
    sendHttpError(response, error);
  } else {
    const serverHttpError = toServerHttpError(error);
    sendHttpError(response, serverHttpError, serverHttpError.headers);
  }
}

function sendHttpError(
  response: Response,
  httpError: HttpError,
  headers?: ServerHttpErrorHeaders,
): void {
  if (headers) {
    response.set(headers);
  }
  response.status(httpError.statusCode).json(toHttpErrorBody(httpError));
}

function toServerHttpError(unexpectedError: unknown): ServerHttpError {
  const options = { includeErrorDetails: config.isDevelopment };

  // `express` uses `http-errors` package to create errors
  const serverHttpError = httpErrors.isHttpErrorsError(unexpectedError)
    ? httpErrors.toWaspHttpError(unexpectedError, options)
    : unknownErrors.toWaspHttpError(unexpectedError, options);

  if (serverHttpError.statusCode >= 500) {
    reportUnexpectedServerError(unexpectedError, { source: "request" });
  }
  return serverHttpError;
}

function formatContext({ source, ...details }: ServerErrorContext): string {
  return [source, ...Object.values(details)].join(" ");
}
