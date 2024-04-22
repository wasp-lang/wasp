{{={= =}=}}
import { prisma } from 'wasp/server'
import { type ProviderId } from 'wasp/auth/utils'
import { type AuthHookFn } from 'wasp/auth/hooks'

{=# onUserSignUpHook.isDefined =}
{=& onUserSignUpHook.importStatement =}
export const onUserSignUpHook = addExtraParamsToTheHookCall({= onUserSignUpHook.importIdentifier =})
{=/ onUserSignUpHook.isDefined =}
{=^ onUserSignUpHook.isDefined =}
export const onUserSignUpHook = undefined
{=/ onUserSignUpHook.isDefined =}

{=# onUserLoginHook.isDefined =}
{=& onUserLoginHook.importStatement =}
export const onUserLoginHook = addExtraParamsToTheHookCall({= onUserLoginHook.importIdentifier =})
{=/ onUserLoginHook.isDefined =}
{=^ onUserLoginHook.isDefined =}
export const onUserLoginHook = undefined
{=/ onUserLoginHook.isDefined =}

{=# onUserLogoutHook.isDefined =}
{=& onUserLogoutHook.importStatement =}
export const onUserLogoutHook = addExtraParamsToTheHookCall({= onUserLogoutHook.importIdentifier =})
{=/ onUserLogoutHook.isDefined =}
{=^ onUserLogoutHook.isDefined =}
export const onUserLogout = undefined
{=/ onUserLogout.isDefined =}

type InternalAuthHookFn = ({ providerId }: { providerId: ProviderId }) => Promise<void> | void

function addExtraParamsToTheHookCall(hookFn: AuthHookFn): InternalAuthHookFn {
  return ({ providerId }) => {
    return hookFn({
      providerId,
      prisma,
    })
  }
}
