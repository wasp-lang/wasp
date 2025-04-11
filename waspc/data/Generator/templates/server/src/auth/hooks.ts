{{={= =}=}}
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
{=# onBeforeSignupHook.isDefined =}
{=& onBeforeSignupHook.importStatement =}
{=/ onBeforeSignupHook.isDefined =}
{=# onAfterSignupHook.isDefined =}
{=& onAfterSignupHook.importStatement =}
{=/ onAfterSignupHook.isDefined =}
{=# onAfterEmailVerifiedHook.isDefined =}
{=& onAfterEmailVerifiedHook.importStatement =}
{=/ onAfterEmailVerifiedHook.isDefined =}
{=# onBeforeOAuthRedirectHook.isDefined =}
{=& onBeforeOAuthRedirectHook.importStatement =}
{=/ onBeforeOAuthRedirectHook.isDefined =}
{=# onBeforeLoginHook.isDefined =}
{=& onBeforeLoginHook.importStatement =}
{=/ onBeforeLoginHook.isDefined =}
{=# onAfterLoginHook.isDefined =}
{=& onAfterLoginHook.importStatement =}
{=/ onAfterLoginHook.isDefined =}

/*
  These are "internal hook functions" based on the user defined hook functions.

  In the server code (e.g. email signup) we import these functions and call them.

  We want to pass extra params to the user defined hook functions, but we don't want to
  pass them when we call them in the server code.
*/

{=# onBeforeSignupHook.isDefined =}
export const onBeforeSignupHook: InternalFunctionForHook<OnBeforeSignupHook> = (params) =>
  {= onBeforeSignupHook.importIdentifier =}({
    prisma,
    ...params,
  })
{=/ onBeforeSignupHook.isDefined =}
{=^ onBeforeSignupHook.isDefined =}
/**
 * This is a no-op function since the user didn't define the onBeforeSignup hook.
 */
export const onBeforeSignupHook: InternalFunctionForHook<OnBeforeSignupHook> = async (_params) => {}
{=/ onBeforeSignupHook.isDefined =}

{=# onAfterSignupHook.isDefined =}
export const onAfterSignupHook: InternalFunctionForHook<OnAfterSignupHook> = (params) =>
  {= onAfterSignupHook.importIdentifier =}({
    prisma,
    ...params,
  })
{=/ onAfterSignupHook.isDefined =}
{=^ onAfterSignupHook.isDefined =}
/**
 * This is a no-op function since the user didn't define the onAfterSignup hook.
 */
export const onAfterSignupHook: InternalFunctionForHook<OnAfterSignupHook> = async (_params) => {}
{=/ onAfterSignupHook.isDefined =}

{=# onAfterEmailVerifiedHook.isDefined =}
export const onAfterEmailVerifiedHook: InternalFunctionForHook<OnAfterEmailVerifiedHook> = (params) =>
  {= onAfterEmailVerifiedHook.importIdentifier =}({
    prisma,
    ...params,
  })
{=/ onAfterEmailVerifiedHook.isDefined =}
{=^ onAfterEmailVerifiedHook.isDefined =}
/**
 * This is a no-op function since the user didn't define the onAfterSignup hook.
 */
export const onAfterEmailVerifiedHook: InternalFunctionForHook<OnAfterEmailVerifiedHook> = async (_params) => {}
{=/ onAfterEmailVerifiedHook.isDefined =}

{=# onBeforeOAuthRedirectHook.isDefined =}
export const onBeforeOAuthRedirectHook: InternalFunctionForHook<OnBeforeOAuthRedirectHook> = (params) =>
  {= onBeforeOAuthRedirectHook.importIdentifier =}({
    prisma,
    ...params,
  })
{=/ onBeforeOAuthRedirectHook.isDefined =}
{=^ onBeforeOAuthRedirectHook.isDefined =}
/**
 * This is an identity function since the user didn't define the onBeforeOAuthRedirect hook.
 */
export const onBeforeOAuthRedirectHook: InternalFunctionForHook<OnBeforeOAuthRedirectHook> = async (params) => params
{=/ onBeforeOAuthRedirectHook.isDefined =}


{=# onBeforeLoginHook.isDefined =}
export const onBeforeLoginHook: InternalFunctionForHook<OnBeforeLoginHook> = (params) =>
  {= onBeforeLoginHook.importIdentifier =}({
    prisma,
    ...params,
  })
{=/ onBeforeLoginHook.isDefined =}
{=^ onBeforeLoginHook.isDefined =}
/**
 * This is a no-op function since the user didn't define the onBeforeLogin hook.
 */
export const onBeforeLoginHook: InternalFunctionForHook<OnBeforeLoginHook> = async (_params) => {}
{=/ onBeforeLoginHook.isDefined =}

{=# onAfterLoginHook.isDefined =}
export const onAfterLoginHook: InternalFunctionForHook<OnAfterLoginHook> = (params) =>
  {= onAfterLoginHook.importIdentifier =}({
    prisma,
    ...params,
  })
{=/ onAfterLoginHook.isDefined =}
{=^ onAfterLoginHook.isDefined =}
/**
 * This is a no-op function since the user didn't define the onAfterLogin hook.
 */
export const onAfterLoginHook: InternalFunctionForHook<OnAfterLoginHook> = async (_params) => {}
{=/ onAfterLoginHook.isDefined =}

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
