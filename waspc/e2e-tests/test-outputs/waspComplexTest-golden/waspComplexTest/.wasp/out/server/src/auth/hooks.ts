import { prisma } from 'wasp/server'
import type {
  OnAfterSignupHook,
  OnAfterEmailVerifiedHook,
  OnBeforeOAuthRedirectHook,
  OnBeforeSignupHook,
  OnBeforeLoginHook,
  OnAfterLoginHook,
  InternalAuthHookParams,
} from 'wasp/server/auth'
import { onBeforeSignup as onBeforeSignupHook_ext } from '../../../../../src/auth/hooks.js'
import { onAfterSignup as onAfterSignupHook_ext } from '../../../../../src/auth/hooks.js'
import { onBeforeOAuthRedirect as onBeforeOAuthRedirectHook_ext } from '../../../../../src/auth/hooks.js'

/*
  These are "internal hook functions" based on the user defined hook functions.

  In the server code (e.g. email signup) we import these functions and call them.

  We want to pass extra params to the user defined hook functions, but we don't want to
  pass them when we call them in the server code.
*/

export const onBeforeSignupHook: InternalFunctionForHook<OnBeforeSignupHook> = (params) =>
  onBeforeSignupHook_ext({
    prisma,
    ...params,
  })

export const onAfterSignupHook: InternalFunctionForHook<OnAfterSignupHook> = (params) =>
  onAfterSignupHook_ext({
    prisma,
    ...params,
  })

/**
 * This is a no-op function since the user didn't define the onAfterSignup hook.
 */
export const onAfterEmailVerifiedHook: InternalFunctionForHook<OnAfterEmailVerifiedHook> = async (_params) => {}

export const onBeforeOAuthRedirectHook: InternalFunctionForHook<OnBeforeOAuthRedirectHook> = (params) =>
  onBeforeOAuthRedirectHook_ext({
    prisma,
    ...params,
  })


/**
 * This is a no-op function since the user didn't define the onBeforeLogin hook.
 */
export const onBeforeLoginHook: InternalFunctionForHook<OnBeforeLoginHook> = async (_params) => {}

/**
 * This is a no-op function since the user didn't define the onAfterLogin hook.
 */
export const onAfterLoginHook: InternalFunctionForHook<OnAfterLoginHook> = async (_params) => {}

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
