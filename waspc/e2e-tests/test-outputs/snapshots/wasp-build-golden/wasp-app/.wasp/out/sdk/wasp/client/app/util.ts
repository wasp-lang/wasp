import { useSyncExternalStore } from "react"

/**
 * Returns `true` if the component is running on the client (browser) and
 * `false` if on the server (SSR). Doesn't cause hydration mismatches, so it is
 * safe to use for conditional rendering.
 */
export function useIsClient() {
  // We use `useSyncExternalStore` to get a value that is `true` on the client
  // and `false` on the server, while avoiding hydration mismatches. It looks
  // like a hack, but it conforms to the semantics of `useSyncExternalStore`,
  // with *the environment* being the "external store" in this case. We just
  // don't have any real subscription logic, since the value is static.
  return useSyncExternalStore(emptySubscribe, getClientValue, getServerValue)
}

// These functions are just to satisfy the API of `useSyncExternalStore` in
// `useIsClient`, and defined outside the hook so they are stable references.
function emptySubscribe() { return emptyUnsubscribe }
function emptyUnsubscribe() {}
function getClientValue() { return true }
function getServerValue() { return false }
