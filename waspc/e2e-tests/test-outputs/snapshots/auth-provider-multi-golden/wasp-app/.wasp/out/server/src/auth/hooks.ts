import { prisma } from 'wasp/server'
import type {
  OnAfterEmailVerifiedHook,
  OnBeforeOAuthRedirectHook,
  InternalAuthHookParams,
} from 'wasp/server/auth'

/*
  Only the method-specific hooks of Wasp's own auth live here. The generic
  lifecycle hooks (onBeforeSignup, onAfterSignup, onBeforeLogin, onAfterLogin)
  are app-level (`auth.hooks`) and fire at Wasp-owned choke points in the SDK
  (see 'wasp/server/auth/hookDispatch'), so they cover every provider.

  These are "internal hook functions" based on the user defined hook functions:
  we pass extra params (prisma) to the user's functions without requiring the
  call sites to supply them.
*/

/**
 * This is a no-op function since the user didn't define the onAfterEmailVerified hook.
 */
export const onAfterEmailVerifiedHook: InternalFunctionForHook<OnAfterEmailVerifiedHook> = async (_params) => {}

/**
 * This is an identity function since the user didn't define the onBeforeOAuthRedirect hook.
 */
export const onBeforeOAuthRedirectHook: InternalFunctionForHook<OnBeforeOAuthRedirectHook> = async (params) => params

/*
  We pass extra params to the user defined hook functions, but we don't want to
  pass the extra params (e.g. 'prisma') when we call the hooks in the server code.
  So, we need to remove the extra params from the params object which is used to define the
  internal hook functions.
*/
type InternalFunctionForHook<Fn extends (args: never) => unknown | Promise<unknown>> = Fn extends (
  params: infer P,
) => infer R
  ? (args: Omit<P, keyof InternalAuthHookParams>) => R
  : never
