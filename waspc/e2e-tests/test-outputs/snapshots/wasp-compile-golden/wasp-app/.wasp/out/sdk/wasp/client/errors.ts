// PRIVATE API (client)
/**
 * The one place where the client logs errors it didn't expect.
 * `context` says where the error happened, e.g. `"OAuth callback"`.
 */
export function reportClientError(error: unknown, context: string): void {
  console.error(`Unexpected error in ${context}:`, error)
}
