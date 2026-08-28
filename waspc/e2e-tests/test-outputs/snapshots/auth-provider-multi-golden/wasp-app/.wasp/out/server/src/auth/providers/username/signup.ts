import { defineHandler } from 'wasp/server/utils'
import {
  createProviderId,
  rethrowPossibleAuthError,
} from 'wasp/server/auth/utils'
import { getIdentityStore } from 'wasp/server/auth/identityStore'
import { hashPassword } from 'wasp/server/auth/password'
import {
  ensureValidUsername,
  ensurePasswordIsPresent,
  ensureValidPassword,
} from 'wasp/auth/validation'
import { validateAndGetUserFields } from 'wasp/server/auth/utils'
import type { UserSignupFields } from 'wasp/auth/providers/types'
import { onBeforeSignupHook, onAfterSignupHook } from '../../hooks.js';

export function getSignupRoute({
  userSignupFields,
}: {
  userSignupFields?: UserSignupFields;
}) {
  return defineHandler(async function signup(req, res) {
    const fields = req.body ?? {}
    ensureValidArgs(fields)

    const providerId = createProviderId('username', fields.username)

    // The hook runs first so it can veto the signup (by throwing) before the
    // developer's `userSignupFields` getters run.
    try {
      await onBeforeSignupHook({ req, providerId })
    } catch (e: unknown) {
      rethrowPossibleAuthError(e)
    }

    const userFields = await validateAndGetUserFields(
      fields,
      userSignupFields,
    );

    try {
      const user = await getIdentityStore('username').createIdentity(
        fields.username,
        {
          // Hashing is the flow's explicit job -- storage never hashes.
          secrets: {
            hashedPassword: await hashPassword(fields.password),
          },
        },
        // Using any here because we want to avoid TypeScript errors and
        // rely on Prisma to validate the data.
        userFields as any
      )
      await onAfterSignupHook({ req, providerId, user })
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
