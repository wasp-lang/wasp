import { isHTTPError as isKyHttpError } from "ky";
import { HttpError } from "../HttpError.js";

export type ClientErrorContext =
  | { source: "render"; componentStack?: string }
  | { source: "window" }
  | { source: "oauthCallback" }
  | { source: "authForm" }
  | { source: "optimisticUpdate" };

// The handler can receive the same error more than once.
const handledErrors = new WeakSet<object>();

/**
 * Reports a client error if it is unexpected, once per error object.
 *
 * An {@link HttpError} or ky's `HTTPError` with a status below 500 is
 * expected, because it comes from an error response that the server sent on
 * purpose.
 *
 * @param error The thrown value.
 * @param context Where the error surfaced.
 */
export function reportClientError(
  error: unknown,
  context: ClientErrorContext,
): void {
  if (isExpectedHttpError(error)) {
    return;
  }
  if (typeof error === "object" && error !== null) {
    if (handledErrors.has(error)) {
      return;
    }
    handledErrors.add(error);
  }
  reportUnexpectedClientError(error, context);
}

function isExpectedHttpError(error: unknown): boolean {
  if (error instanceof HttpError) {
    return error.status < 500;
  }
  // Users who call `api` without `handleApiError` get ky's error instead of
  // an `HttpError`. ky also throws it for statuses below 400, e.g. for a
  // redirect that wasn't followed.
  return isKyHttpError(error) && error.response.status < 500;
}

/**
 * Passes the errors that reach the window to {@link reportClientError}, e.g.
 * errors thrown in event handlers and rejected promises that nobody awaited.
 *
 * @see https://developer.mozilla.org/en-US/docs/Web/API/Window/error_event
 * @see https://developer.mozilla.org/en-US/docs/Web/API/Window/unhandledrejection_event
 */
export function listenForUncaughtClientErrors(): void {
  window.addEventListener("error", (event: ErrorEvent) => {
    reportClientError(event.error ?? event.message, { source: "window" });
  });
  window.addEventListener(
    "unhandledrejection",
    (event: PromiseRejectionEvent) => {
      reportClientError(event.reason, { source: "window" });
    },
  );
}

// TODO: Refactor this once Wasp has proper logging.
function reportUnexpectedClientError(
  error: unknown,
  context: ClientErrorContext,
): void {
  console.error(`Unexpected error in ${context.source}:`, error);
  if (context.source === "render" && context.componentStack) {
    console.error(`The error happened in:${context.componentStack}`);
  }
}
