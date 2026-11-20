// This file must not import the server's config or env. The server installs
// its handlers for uncaught errors through it before any user code loads, and
// the env imports the user's validation schema.
import type {
  ServerErrorContext,
  ServerOnErrorFn,
} from "@wasp/lib-sdk-core/node";
import { setTimeout as sleep } from "node:timers/promises";

const MAX_ON_ERROR_FN_WAIT_MS = 5000;

let serverOnErrorFn: ServerOnErrorFn | undefined;

// PRIVATE API (server)
export function registerServerOnErrorFn(onErrorFn: ServerOnErrorFn): void {
  serverOnErrorFn = onErrorFn;
}

// PRIVATE API (server)
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

// PRIVATE API (server)
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
