import { defineHandler } from 'wasp/server/utils'
import { rethrowPossibleAuthError } from 'wasp/server/auth/utils'
import { waspAuthRuntime } from 'wasp/server/auth/provider'
import { hashPassword } from 'wasp/server/auth/password'
import {
  ensureValidUsername,
  ensurePasswordIsPresent,
  ensureValidPassword,
} from 'wasp/auth/validation'
import { validateAndGetUserFields } from 'wasp/server/auth/utils'
import type { UserSignupFields } from 'wasp/auth/providers/types'

export function getSignupRoute({
  userSignupFields,
}: {
  userSignupFields?: UserSignupFields;
}) {
  return defineHandler(async function signup(req, res) {
    const fields = req.body ?? {}
    ensureValidArgs(fields)

    try {
      // The identity facet's `create` is the signup choke point: the app's
      // onBeforeSignup veto fires first, then the lazy `userSignupFields`
      // getters, then the atomic write, then onAfterSignup.
      await waspAuthRuntime.identityNamespaces('username').create(
        fields.username,
        {
          // Hashing is the flow's explicit job -- storage never hashes.
          secrets: {
            hashedPassword: await hashPassword(fields.password),
          },
        },
        // Using any because we want to rely on Prisma to validate the data.
        (() => validateAndGetUserFields(fields, userSignupFields)) as any,
        { req },
      )
    } catch (e: unknown) {
      rethrowPossibleAuthError(e)
    }

    res.json({ success: true })
  })
}

function ensureValidArgs(args: object): void {
  ensureValidUsername(args)
  ensurePasswordIsPresent(args)
  ensureValidPassword(args)
}
