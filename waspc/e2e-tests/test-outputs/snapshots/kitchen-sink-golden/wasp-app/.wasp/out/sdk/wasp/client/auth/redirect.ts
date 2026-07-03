import {
  clearOriginalRoute,
  consumeOriginalRoute,
  peekOriginalRoute,
  type OnAuthSucceededRedirectFn,
} from '@wasp.sh/lib-auth/browser'
import { onAuthSucceededRedirect as onAuthSucceededRedirect_ext } from 'wasp/src/features/auth/clientHooks'

const onAuthSucceededRedirectFn: OnAuthSucceededRedirectFn = (context) =>
  onAuthSucceededRedirect_ext(context)

// PRIVATE API
/**
 * Consumes the saved original route (it is used at most once), so call this
 * from event handlers only, right before navigating.
 */
export function consumeOnAuthSucceededRedirectRoute(): string {
  return onAuthSucceededRedirectFn({ originalRoute: consumeOriginalRoute() })
}

// PRIVATE API
/**
 * Storage-free variant for render code: pass a route obtained earlier via
 * `peekOriginalRoute` and clear it separately once navigation is committed.
 */
export function getOnAuthSucceededRedirectRouteFor(
  originalRoute: string | undefined,
): string {
  return onAuthSucceededRedirectFn({ originalRoute })
}

// PRIVATE API
export { clearOriginalRoute, peekOriginalRoute }
