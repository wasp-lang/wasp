import { prisma } from 'wasp/server'
import type {
  OnAfterOAuthTokenReceivedHookFn,
  OnAfterSignupHookFn,
  OnBeforeOAuthRedirectHookFn,
  OnBeforeSignupHookFn,
} from 'wasp/server/auth'

/*
  These are "internal hook functions" based on the user defined hook functions.

  In the server code (e.g. email signup) we import these functions and call them.

  We want to pass extra params to the user defined hook functions, but we don't want to
  pass 'prisma' and 'hookName' to them when we call them in the server code.
*/

export const onBeforeSignupHook = undefined

export const onAfterSignupHook = undefined

export const onBeforeOAuthRedirectHook = undefined

export const onAfterOAuthTokenReceivedHook = undefined

/*
  We pass extra params to the user defined hook functions, but we don't want to
  pass 'prisma' and 'hookName' to them when we call them in the server code.
  So, we need to remove them from the params object which is used to define the
  internal hook functions.
*/
type UserHookParamsToInternalHookParams<T> = T extends (
  params: infer P,
) => unknown
  ? Omit<P, 'prisma' | 'hookName'>
  : never
