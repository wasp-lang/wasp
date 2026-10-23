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
 * - An `HttpError` is thrown on purpose, so we send its status code,
 *   headers, message, and data.
 * - Errors from Express and other middleware keep their status code and
 *   headers, the same as with Express's default error handler. We only send
 *   their message when they mark it as safe to show.
 * - Other errors keep their status code and headers if they carry a valid
 *   status code, like with Express's default error handler, or become a 500.
 *   We never send their message, so we don't leak internal details to the
 *   client.
 *
 * We report every error that ends up as a 5xx.
 *
 * In development, a hidden error's message and stack go into `data`, like
 * Express's default error handler shows them on its HTML page.
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
