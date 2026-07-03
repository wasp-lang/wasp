{{={= =}=}}
import {
  clearOriginalRoute,
  consumeOriginalRoute,
  peekOriginalRoute,
{=^ onAuthSucceededRedirectFn.isDefined =}
  redirectToFixed,
{=/ onAuthSucceededRedirectFn.isDefined =}
  type OnAuthSucceededRedirectFn,
} from '@wasp.sh/lib-auth/browser'
{=# onAuthSucceededRedirectFn.isDefined =}
{=& onAuthSucceededRedirectFn.importStatement =}
{=/ onAuthSucceededRedirectFn.isDefined =}

{=^ onAuthSucceededRedirectFn.isDefined =}
const onAuthSucceededRedirectFn: OnAuthSucceededRedirectFn = redirectToFixed('/')
{=/ onAuthSucceededRedirectFn.isDefined =}
{=# onAuthSucceededRedirectFn.isDefined =}
{=!
// The user's function is referenced lazily (not copied into a top-level
// const) so this module can be evaluated before the user's module is
// initialized — the two can be part of an import cycle.
=}
const onAuthSucceededRedirectFn: OnAuthSucceededRedirectFn = (context) =>
  {= onAuthSucceededRedirectFn.importIdentifier =}(context)
{=/ onAuthSucceededRedirectFn.isDefined =}

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
