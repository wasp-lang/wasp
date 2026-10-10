// PRIVATE API (client)
/**
 * Where an unexpected client error happened.
 *
 * It's structured data instead of a plain string, like the server's
 * `ServerErrorContext`, so we can later group errors by `source`.
 */
export type ClientErrorContext =
  | { source: "pageRender" }
  | { source: "oauthCallback" }
  | { source: "authForm" }
  | { source: "optimisticUpdate" };

// PRIVATE API (client)
/**
 * The one place where the client logs errors it didn't expect.
 */
export function reportClientError(
  error: unknown,
  context: ClientErrorContext,
): void {
  console.error(`Unexpected error in ${context.source}:`, error);
}
