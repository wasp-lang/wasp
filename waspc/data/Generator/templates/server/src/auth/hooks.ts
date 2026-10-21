{{={= =}=}}
import { createAuthHooks } from '@wasp.sh/lib-auth/node'
import { prisma } from 'wasp/server'
import type { AuthHookTypes } from 'wasp/server/auth'
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

  In the auth flows (e.g. email signup) we call these functions.

  They pass extra params (e.g. `prisma`) to the user defined hook functions, and
  are no-ops for the hooks the user didn't define.
*/
export const {
  onBeforeSignupHook,
  onAfterSignupHook,
  onAfterEmailVerifiedHook,
  onBeforeOAuthRedirectHook,
  onBeforeLoginHook,
  onAfterLoginHook,
} = createAuthHooks<AuthHookTypes>({
  prisma,
  hooks: {
    {=# onBeforeSignupHook.isDefined =}
    onBeforeSignup: {= onBeforeSignupHook.importIdentifier =},
    {=/ onBeforeSignupHook.isDefined =}
    {=# onAfterSignupHook.isDefined =}
    onAfterSignup: {= onAfterSignupHook.importIdentifier =},
    {=/ onAfterSignupHook.isDefined =}
    {=# onAfterEmailVerifiedHook.isDefined =}
    onAfterEmailVerified: {= onAfterEmailVerifiedHook.importIdentifier =},
    {=/ onAfterEmailVerifiedHook.isDefined =}
    {=# onBeforeOAuthRedirectHook.isDefined =}
    onBeforeOAuthRedirect: {= onBeforeOAuthRedirectHook.importIdentifier =},
    {=/ onBeforeOAuthRedirectHook.isDefined =}
    {=# onBeforeLoginHook.isDefined =}
    onBeforeLogin: {= onBeforeLoginHook.importIdentifier =},
    {=/ onBeforeLoginHook.isDefined =}
    {=# onAfterLoginHook.isDefined =}
    onAfterLogin: {= onAfterLoginHook.importIdentifier =},
    {=/ onAfterLoginHook.isDefined =}
  },
})
