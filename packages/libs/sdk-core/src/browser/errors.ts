import { isHTTPError as isKyHttpError } from "ky";
import { HttpError } from "../HttpError.js";

/**
 * The type of the function you pass to the `client.onError` field of the app
 * spec. Wasp calls it with every unexpected error on the client, after
 * logging the error to the browser console.
 *
 * It receives the error and the {@link ClientErrorContext} it surfaced in.
 *
 * - An [`HttpError`](https://wasp.sh/docs/api/wasp/errors#httperror), or the
 *   error the `api` client throws, is unexpected only with a status of 500 or
 *   above. A lower status means the server sent the error on purpose.
 * - A failed Query or Action that your code handles is not unexpected. A
 *   failed Action that nobody catches arrives as a `"window"` error.
 * - Wasp calls the function once per error, even if the error surfaces in
 *   more than one place.
 * - Render errors arrive even when your own error boundaries catch them.
 * - Wasp doesn't call the function while prerendering pages.
 * - If the function throws, Wasp logs that error and keeps running.
 *
 * @example
 * ```ts
 * import type { ClientOnErrorFn } from "wasp/client";
 *
 * export const onClientError: ClientOnErrorFn = (error, context) => {
 *   errorTracker.capture(error, { source: context.source });
 * };
 * ```
 *
 * @see [Handling unexpected errors](https://wasp.sh/docs/advanced/client-customization/client-config#handling-unexpected-errors)
 */
export type ClientOnErrorFn = (
  error: unknown,
  context: ClientErrorContext,
) => void | Promise<void>;

/**
 * Where an unexpected client error surfaced.
 *
 * - `"render"`: While rendering a page or the root component.
 *   `componentStack` lists the components that led to the error, when React
 *   provides it.
 * - `"window"`: In the browser's `error` and `unhandledrejection` events,
 *   e.g. an error thrown in an event handler or a rejected promise nobody
 *   caught. `event` is the name of the browser event.
 * - `"oauthCallback"`: While finishing an OAuth login.
 * - `"authForm"`: While submitting one of Wasp's Auth UI forms.
 * - `"optimisticUpdate"`: In the `updateQuery` function of an optimistic
 *   update.
 */
export type ClientErrorContext =
  | { source: "render"; componentStack?: string }
  | { source: "window"; event: "error" | "unhandledrejection" }
  | { source: "oauthCallback" }
  | { source: "authForm" }
  | { source: "optimisticUpdate" };

let clientOnErrorFn: ClientOnErrorFn | undefined;

export function registerClientOnErrorFn(onErrorFn: ClientOnErrorFn): void {
  clientOnErrorFn = onErrorFn;
}

// The same error can be reported from more than one place.
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
    reportClientError(event.error ?? event.message, {
      source: "window",
      event: "error",
    });
  });
  window.addEventListener(
    "unhandledrejection",
    (event: PromiseRejectionEvent) => {
      reportClientError(event.reason, {
        source: "window",
        event: "unhandledrejection",
      });
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
  void callClientOnErrorFn(error, context);
}

// It never rejects, so a failing `client.onError` function isn't reported as
// another unexpected error.
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
