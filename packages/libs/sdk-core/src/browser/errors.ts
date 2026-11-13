import { isHTTPError } from "ky";
import { HttpError } from "../HttpError.js";

export type ClientErrorContext =
  | { source: "render" }
  | { source: "query" }
  | { source: "action" }
  | { source: "window" }
  | { source: "oauthCallback" }
  | { source: "authForm" }
  | { source: "optimisticUpdate" };

export type ClientOnErrorFn = (
  error: unknown,
  context: ClientErrorContext,
) => void | Promise<void>;

let clientOnErrorFn: ClientOnErrorFn | undefined;

export function registerClientOnErrorFn(onErrorFn: ClientOnErrorFn): void {
  clientOnErrorFn = onErrorFn;
}

// The same error can surface in more than one place, e.g. an action error
// reaches TanStack Query's cache and, if nobody catches it, the window.
const handledErrors = new WeakSet<object>();

/**
 * The client error handler. Wasp calls it from every place where errors
 * surface: the root error boundary, React's root, TanStack Query's caches,
 * the browser's global error events, and Wasp's own `catch` blocks.
 *
 * An HTTP error response is sent by the server on purpose, and the server
 * already reports its own unexpected errors, so we only report everything
 * else.
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
// our `HttpError`.
function isHttpErrorResponse(error: unknown): boolean {
  return error instanceof HttpError || isHTTPError(error);
}

/**
 * Catches errors that nothing else caught, e.g. errors thrown in event
 * handlers and rejected promises that nobody awaited.
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
  void callClientOnErrorFn(error, context);
}

// An error that escaped from here would reach the window and come back to
// the client error handler.
async function callClientOnErrorFn(
  error: unknown,
  context: ClientErrorContext,
): Promise<void> {
  try {
    await clientOnErrorFn?.(error, context);
  } catch (onErrorFnError: unknown) {
    console.error("The `client.onError` function failed:", onErrorFnError);
  }
}
