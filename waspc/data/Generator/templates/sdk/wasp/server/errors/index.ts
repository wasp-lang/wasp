import { HttpError, toHttpErrorBody } from "@wasp/lib-sdk-core";
import type { Response } from "express";
import config from "../config.js";
import * as httpErrors from "./httpErrors.js";
import * as unknownErrors from "./unknownErrors.js";

// PUBLIC API
export type ServerErrorContext =
  | { source: "request" }
  | { source: "job"; jobName: string }
  | { source: "webSocket" }
  | { source: "oauth" }
  | { source: "startup" }
  | { source: "dbSeed" };

// PUBLIC API
export type ServerOnErrorFn = (
  error: unknown,
  context: ServerErrorContext,
) => void | Promise<void>;

let serverOnErrorFn: ServerOnErrorFn | undefined;

// PRIVATE API (server)
export function registerServerOnErrorFn(onErrorFn: ServerOnErrorFn): void {
  serverOnErrorFn = onErrorFn;
}

// PRIVATE API (server)
/**
 * Logs the error and passes it to the user's `server.onError` function.
 * It never rejects, so callers that can't wait for it can ignore the promise.
 */
// TODO: Refactor this once Wasp has proper logging.
export async function reportUnexpectedServerError(
  error: unknown,
  context: ServerErrorContext,
): Promise<void> {
  console.error(`Unexpected error in ${formatContext(context)}:`, error);
  try {
    await serverOnErrorFn?.(error, context);
  } catch (onErrorFnError: unknown) {
    console.error("The `server.onError` function failed:", onErrorFnError);
  }
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
  response.status(httpError.statusCode).json(toHttpErrorBody(httpError));
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
    void reportUnexpectedServerError(error, { source: "request" });
  }
  return httpError;
}

function formatContext({ source, ...details }: ServerErrorContext): string {
  return [source, ...Object.values(details)].join(" ");
}
