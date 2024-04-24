{{={= =}=}}
import { prisma } from 'wasp/server'
import type {
  OnAfterOAuthTokenReceivedHookFn,
  OnAfterSignupHookFn,
  OnBeforeOAuthRedirectHookFn,
  OnBeforeSignupHookFn,
} from 'wasp/server/auth'
{=# onBeforeSignupHook.isDefined =}
{=& onBeforeSignupHook.importStatement =}
{=/ onBeforeSignupHook.isDefined =}
{=# onAfterSignupHook.isDefined =}
{=& onAfterSignupHook.importStatement =}
{=/ onAfterSignupHook.isDefined =}
{=# onBeforeOAuthRedirectHook.isDefined =}
{=& onBeforeOAuthRedirectHook.importStatement =}
{=/ onBeforeOAuthRedirectHook.isDefined =}
{=# onAfterOAuthTokenReceivedHook.isDefined =}
{=& onAfterOAuthTokenReceivedHook.importStatement =}
{=/ onAfterOAuthTokenReceivedHook.isDefined =}

/*
  These are "internal hook functions" based on the user defined hook functions.

  In the server code (e.g. email signup) we import these functions and call them.

  We want to pass extra params to the user defined hook functions, but we don't want to
  pass 'prisma' and 'hookName' to them when we call them in the server code.
*/

{=# onBeforeSignupHook.isDefined =}
export const onBeforeSignupHook = (
  params: UserHookParamsToInternalHookParams<OnBeforeSignupHookFn>,
) =>
  {= onBeforeSignupHook.importIdentifier =}({
    hookName: 'onBeforeSignup',
    prisma,
    ...params,
  })
{=/ onBeforeSignupHook.isDefined =}
{=^ onBeforeSignupHook.isDefined =}
export const onBeforeSignupHook = undefined
{=/ onBeforeSignupHook.isDefined =}

{=# onAfterSignupHook.isDefined =}
export const onAfterSignupHook = (
  params: UserHookParamsToInternalHookParams<OnAfterSignupHookFn>,
) =>
  {= onAfterSignupHook.importIdentifier =}({
    hookName: 'onAfterSignup',
    prisma,
    ...params,
  })
{=/ onAfterSignupHook.isDefined =}
{=^ onAfterSignupHook.isDefined =}
export const onAfterSignupHook = undefined
{=/ onAfterSignupHook.isDefined =}

{=# onBeforeOAuthRedirectHook.isDefined =}
export const onBeforeOAuthRedirectHook = (
  params: UserHookParamsToInternalHookParams<OnBeforeOAuthRedirectHookFn>,
) =>
  {= onBeforeOAuthRedirectHook.importIdentifier =}({
    hookName: 'onBeforeOAuthRedirect',
    prisma,
    ...params,
  })
{=/ onBeforeOAuthRedirectHook.isDefined =}
{=^ onBeforeOAuthRedirectHook.isDefined =}
export const onBeforeOAuthRedirectHook = undefined
{=/ onBeforeOAuthRedirectHook.isDefined =}

{=# onAfterOAuthTokenReceivedHook.isDefined =}
export const onAfterOAuthTokenReceivedHook = (
  params: UserHookParamsToInternalHookParams<OnAfterOAuthTokenReceivedHookFn>,
) =>
  {= onAfterOAuthTokenReceivedHook.importIdentifier =}({
    hookName: 'onAfterOAuthTokenReceived',
    prisma,
    ...params,
  })
{=/ onAfterOAuthTokenReceivedHook.isDefined =}
{=^ onAfterOAuthTokenReceivedHook.isDefined =}
export const onAfterOAuthTokenReceivedHook = undefined
{=/ onAfterOAuthTokenReceivedHook.isDefined =}

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
