/**
 * The type of the function you pass to the `server.onError` field of the app
 * spec. Wasp calls it with every unexpected error on the server, after
 * logging the error.
 *
 * It receives the error and the {@link ServerErrorContext} it happened in.
 *
 * - An [`HttpError`](https://wasp.sh/docs/api/wasp/errors#httperror) you
 *   throw while handling a request is not unexpected, so Wasp doesn't call
 *   the function for it.
 * - The function can be async. Wasp waits for it only when the server exits
 *   because of the error, and for at most 5 seconds. The server exits after
 *   a `"startup"` or `"dbSeed"` error, and after a `"process"` error whose
 *   `event` is `uncaughtException`.
 * - If the function throws, Wasp logs that error and keeps running.
 *
 * @example
 * ```ts
 * import type { ServerOnErrorFn } from "wasp/server";
 *
 * export const onServerError: ServerOnErrorFn = (error, context) => {
 *   errorTracker.capture(error, { source: context.source });
 * };
 * ```
 *
 * @see [Handling unexpected errors](https://wasp.sh/docs/advanced/server-customization/server-config#handling-unexpected-errors)
 */
export type ServerOnErrorFn = (
  error: unknown,
  context: ServerErrorContext,
) => void | Promise<void>;

/**
 * Where an unexpected server error happened.
 *
 * - `"request"`: While handling a request to an Operation, API, or auth route.
 * - `"job"`: In a Job. `jobName` is the name of the Job.
 * - `"webSocket"`: While authenticating a WebSocket connection.
 * - `"oauth"`: During an OAuth login.
 * - `"startup"`: While starting the server, after your code has loaded, e.g.
 *   in `server.setupFn`.
 * - `"dbSeed"`: In a database seed function, when running `wasp db seed`.
 * - `"process"`: Anywhere else in the server, as an uncaught error or an
 *   unhandled promise rejection, e.g. in a timer or while your code loads.
 *   `event` is the name of the Node.js process event.
 */
export type ServerErrorContext =
  | { source: "request" }
  | { source: "job"; jobName: string }
  | { source: "webSocket" }
  | { source: "oauth" }
  | { source: "startup" }
  | { source: "dbSeed" }
  | { source: "process"; event: "uncaughtException" | "unhandledRejection" };

const MAX_ON_ERROR_FN_WAIT_MS = 5000;

let serverOnErrorFn: ServerOnErrorFn | undefined;

export function registerServerOnErrorFn(onErrorFn: ServerOnErrorFn): void {
  serverOnErrorFn = onErrorFn;
}

// TODO: Refactor this once Wasp has proper logging.
/**
 * Logs the error and passes it to the user's `server.onError` function.
 * It never rejects, so callers can ignore the promise.
 */
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

/**
 * Reports an error that the caller exits the process after.
 *
 * It waits for the user's `server.onError` function, so an error tracker can
 * send the error first. It stops waiting after
 * {@link MAX_ON_ERROR_FN_WAIT_MS}, so a function that never finishes can't
 * keep a broken server running.
 */
export async function reportFatalServerError(
  error: unknown,
  context: ServerErrorContext,
): Promise<void> {
  await Promise.race([
    reportUnexpectedServerError(error, context),
    sleep(MAX_ON_ERROR_FN_WAIT_MS),
  ]);
}

function formatContext({ source, ...details }: ServerErrorContext): string {
  return [source, ...Object.values(details)].join(" ");
}

function sleep(milliseconds: number): Promise<void> {
  return new Promise((resolve) => setTimeout(resolve, milliseconds));
}
