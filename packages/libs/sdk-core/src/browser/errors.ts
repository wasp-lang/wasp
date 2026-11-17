import { isHTTPError as isKyHttpError } from "ky";
import { HttpError } from "../HttpError.js";

export type ClientErrorContext =
  | { source: "render" }
  | { source: "query" }
  | { source: "action" }
  | { source: "window" }
  | { source: "oauthCallback" }
  | { source: "authForm" }
  | { source: "optimisticUpdate" };

// The handler can receive the same error more than once.
const handledErrors = new WeakSet<object>();

/**
 * Reports an unexpected client error, once per error object.
 *
 * It skips an {@link HttpError} and ky's `HTTPError`, because both come from
 * an error response that the server sent on purpose.
 *
 * @param error The thrown value.
 * @param context Where the error surfaced.
 */
export function handleClientError(
  error: unknown,
  context: ClientErrorContext,
): void {
  if (isHttpErrorResponse(error)) {
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

// Users who call `api` without `handleApiError` get ky's error instead of
// a `ClientHttpError`.
function isHttpErrorResponse(error: unknown): boolean {
  return error instanceof HttpError || isKyHttpError(error);
}

/**
 * Passes the errors that reach the window to {@link handleClientError}, e.g.
 * errors thrown in event handlers and rejected promises that nobody awaited.
 *
 * @see https://developer.mozilla.org/en-US/docs/Web/API/Window/error_event
 * @see https://developer.mozilla.org/en-US/docs/Web/API/Window/unhandledrejection_event
 */
export function listenForUncaughtClientErrors(): void {
  window.addEventListener("error", (event: ErrorEvent) => {
    handleClientError(event.error ?? event.message, { source: "window" });
  });
  window.addEventListener(
    "unhandledrejection",
    (event: PromiseRejectionEvent) => {
      handleClientError(event.reason, { source: "window" });
    },
  );
}

// TODO: Refactor this once Wasp has proper logging.
function reportUnexpectedClientError(
  error: unknown,
  context: ClientErrorContext,
): void {
  console.error(`Unexpected error in ${context.source}:`, error);
}
