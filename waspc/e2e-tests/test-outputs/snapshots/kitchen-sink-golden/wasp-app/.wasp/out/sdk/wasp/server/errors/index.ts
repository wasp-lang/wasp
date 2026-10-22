import { getErrorMessage, HttpError } from "@wasp.sh/lib-sdk-core";
import type { Response } from "express";
import { STATUS_CODES } from "node:http";
import config from "../config.js";
import * as httpErrors from "./httpErrors.js";

// PRIVATE API (server)
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
 *   message, and data.
 * - Errors from Express and other middleware keep their status code and
 *   headers, the same as with Express's default error handler. We only send
 *   their message when they mark it as safe to show.
 * - We report everything else and hide it behind a generic 500, so we don't
 *   leak internal details to the client.
 *
 * In development, a hidden error's message and stack go into `data`, like
 * Express's default error handler shows them on its HTML page.
 */
export function sendErrorResponse(response: Response, error: unknown): void {
  const httpError = toHttpError(error);
  const headers = httpErrors.getHeaders(error);
  if (headers) {
    response.set(headers);
  }
  response
    .status(httpError.statusCode)
    .json({ message: httpError.message, data: httpError.data });
}

function toHttpError(error: unknown): HttpError {
  if (error instanceof HttpError) {
    return error;
  }

  const statusCode = httpErrors.getStatusCode(error) ?? 500;
  if (statusCode >= 500) {
    reportUnexpectedServerError(error, { source: "request" });
  }

  if (httpErrors.isMessageExposed(error)) {
    return new HttpError(statusCode, error.message);
  }
  return new HttpError(
    statusCode,
    STATUS_CODES[statusCode],
    config.isDevelopment ? getDevelopmentErrorDetails(error) : undefined,
  );
}

function getDevelopmentErrorDetails(error: unknown): {
  message: string;
  stack: string | undefined;
} {
  return {
    message: getErrorMessage(error),
    stack: error instanceof Error ? error.stack : undefined,
  };
}

function formatContext({ source, ...details }: ServerErrorContext): string {
  return [source, ...Object.values(details)].join(" ");
}
