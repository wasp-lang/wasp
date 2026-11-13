import { HttpError } from "../HttpError.js";

export type ClientErrorContext =
  | { source: "render" }
  | { source: "query" }
  | { source: "action" }
  | { source: "window" }
  | { source: "oauthCallback" }
  | { source: "authForm" }
  | { source: "optimisticUpdate" };

// The same error can surface in more than one place, e.g. an action error
// reaches TanStack Query's cache and, if nobody catches it, the window.
const handledErrors = new WeakSet<object>();

/**
 * The client error handler. Wasp calls it from every place where errors
 * surface: the root error boundary, React's root, TanStack Query's caches,
 * the browser's global error events, and Wasp's own `catch` blocks.
 *
 * An `HttpError` is sent by the server on purpose, and the server already
 * reports its own unexpected errors, so we only report everything else.
 */
export function handleClientError(
  error: unknown,
  context: ClientErrorContext,
): void {
  if (error instanceof HttpError) {
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
}
