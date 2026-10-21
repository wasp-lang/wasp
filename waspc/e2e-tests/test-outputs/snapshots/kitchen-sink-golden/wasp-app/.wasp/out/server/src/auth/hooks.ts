import { createAuthHooks } from '@wasp.sh/lib-auth/node'
import { prisma } from 'wasp/server'
import type { AuthHookTypes } from 'wasp/server/auth'
import { onBeforeSignup as onBeforeSignupHook_ext } from "../../../../../src/features/auth/hooks"
import { onAfterSignup as onAfterSignupHook_ext } from "../../../../../src/features/auth/hooks"
import { onAfterEmailVerified as onAfterEmailVerifiedHook_ext } from "../../../../../src/features/auth/hooks"
import { onBeforeLogin as onBeforeLoginHook_ext } from "../../../../../src/features/auth/hooks"
import { onAfterLogin as onAfterLoginHook_ext } from "../../../../../src/features/auth/hooks"

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
    onBeforeSignup: onBeforeSignupHook_ext,
    onAfterSignup: onAfterSignupHook_ext,
    onAfterEmailVerified: onAfterEmailVerifiedHook_ext,
    onBeforeLogin: onBeforeLoginHook_ext,
    onAfterLogin: onAfterLoginHook_ext,
  },
})
