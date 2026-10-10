// PRIVATE API (client)
export type ClientErrorContext =
  | { source: "pageRender" }
  | { source: "oauthCallback" }
  | { source: "authForm" }
  | { source: "optimisticUpdate" };

// PRIVATE API (client)
// TODO: Refactor this once Wasp has proper logging.
export function reportUnexpectedClientError(
  error: unknown,
  context: ClientErrorContext,
): void {
  console.error(`Unexpected error in ${context.source}:`, error);
}
