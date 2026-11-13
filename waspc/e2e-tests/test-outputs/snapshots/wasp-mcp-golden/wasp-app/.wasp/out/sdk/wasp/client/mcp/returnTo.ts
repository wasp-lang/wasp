// The MCP consent page is the OAuth authorization endpoint, so its URL holds
// the whole authorization request; the confirm page's URL holds the pending
// action. When a logged-out user lands on either, it's remembered here and
// restored after login, signup, or a social login. The pages clear it, and it
// expires, so an abandoned flow can't redirect a later login.
const RETURN_TO_KEY = 'wasp:mcpReturnTo'
const RETURN_PATHS: string[] = [
  "/connect",
  "/confirm",
]
const MAX_AGE_MS = 10 * 60 * 1000

// PRIVATE API (used in the SDK)
export function rememberMcpReturnTo(location: { pathname: string; search: string }): void {
  if (!RETURN_PATHS.includes(location.pathname)) return
  try {
    sessionStorage.setItem(
      RETURN_TO_KEY,
      JSON.stringify({ path: location.pathname + location.search, at: Date.now() }),
    )
  } catch {
    // Storage unavailable: the user lands on the default page after login.
  }
}

// PRIVATE API (used in the SDK)
export function peekMcpReturnTo(): string | null {
  try {
    const stored = JSON.parse(sessionStorage.getItem(RETURN_TO_KEY) ?? 'null')
    if (
      stored &&
      typeof stored.path === 'string' &&
      RETURN_PATHS.some((path) => stored.path.startsWith(path + '?')) &&
      Date.now() - stored.at < MAX_AGE_MS
    ) {
      return stored.path
    }
  } catch {
    // Missing or malformed: nothing to return to.
  }
  return null
}

// PRIVATE API (used in the SDK)
export function clearMcpReturnTo(): void {
  try {
    sessionStorage.removeItem(RETURN_TO_KEY)
  } catch {
    // Storage unavailable: nothing was stored.
  }
}
