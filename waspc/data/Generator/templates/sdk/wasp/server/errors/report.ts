// This file must not import the server's config or env. The server installs
// its handlers for uncaught errors through it before any user code loads, and
// the env imports the user's validation schema.

// PRIVATE API (server)
// TODO: Refactor this once Wasp has proper logging.
export type ServerErrorContext =
  | { source: "request" }
  | { source: "job"; jobName: string }
  | { source: "webSocket" }
  | { source: "oauth" }
  | { source: "startup" }
  | { source: "dbSeed" }
  | { source: "process"; event: "uncaughtException" | "unhandledRejection" };

// PRIVATE API (server)
// TODO: Refactor this once Wasp has proper logging.
export function reportUnexpectedServerError(
  error: unknown,
  context: ServerErrorContext,
): void {
  console.error(`Unexpected error in ${formatContext(context)}:`, error);
}

function formatContext({ source, ...details }: ServerErrorContext): string {
  return [source, ...Object.values(details)].join(" ");
}
