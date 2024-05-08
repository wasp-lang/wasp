import { prisma } from 'wasp/server'
import type {
  OnAfterOAuthTokenReceivedHookFn,
  OnAfterSignupHookFn,
  OnBeforeOAuthRedirectHookFn,
  OnBeforeSignupHookFn,
} from 'wasp/server/auth'
import { onBeforeSignup as onBeforeSignupHook_ext } from '../../../../../src/auth/hooks.js'
import { onAfterSignup as onAfterSignupHook_ext } from '../../../../../src/auth/hooks.js'
import { onBeforeOAuthRedirect as onBeforeOAuthRedirectHook_ext } from '../../../../../src/auth/hooks.js'
import { onAfterOAuthTokenReceived as onAfterOAuthTokenReceivedHook_ext } from '../../../../../src/auth/hooks.js'

/*
  These are "internal hook functions" based on the user defined hook functions.

  In the server code (e.g. email signup) we import these functions and call them.

  We want to pass extra params to the user defined hook functions, but we don't want to
  pass 'prisma' and 'hookName' to them when we call them in the server code.
*/

export const onBeforeSignupHook = (
  params: UserHookParamsToInternalHookParams<OnBeforeSignupHookFn>,
) =>
  onBeforeSignupHook_ext({
    hookName: 'onBeforeSignup',
    prisma,
    ...params,
  })

export const onAfterSignupHook = (
  params: UserHookParamsToInternalHookParams<OnAfterSignupHookFn>,
) =>
  onAfterSignupHook_ext({
    hookName: 'onAfterSignup',
    prisma,
    ...params,
  })

export const onBeforeOAuthRedirectHook = (
  params: UserHookParamsToInternalHookParams<OnBeforeOAuthRedirectHookFn>,
) =>
  onBeforeOAuthRedirectHook_ext({
    hookName: 'onBeforeOAuthRedirect',
    prisma,
    ...params,
  })

export const onAfterOAuthTokenReceivedHook = (
  params: UserHookParamsToInternalHookParams<OnAfterOAuthTokenReceivedHookFn>,
) =>
  onAfterOAuthTokenReceivedHook_ext({
    hookName: 'onAfterOAuthTokenReceived',
    prisma,
    ...params,
  })

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
