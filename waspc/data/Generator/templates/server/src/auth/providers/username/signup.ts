{{={= =}=}}
import { defineHandler } from 'wasp/server/utils'
import { signupWithUsername } from '@wasp.sh/lib-auth/node'
import {
  createUser,
  rethrowPossibleAuthError,
  rethrowPossibleAuthServiceError,
  validateAndGetUserFields,
} from 'wasp/auth/utils'
import type { UserSignupFields } from 'wasp/auth/providers/types'
import { onBeforeSignupHook, onAfterSignupHook } from '../../hooks.js';

export function getSignupRoute({
  userSignupFields,
}: {
  userSignupFields?: UserSignupFields;
}) {
  return defineHandler(async function signup(req, res) {
    const fields = req.body ?? {}

    try {
      await signupWithUsername({
        fields,
        request: req,
        getUserFields: (fields) =>
          validateAndGetUserFields(
            fields as Record<string, unknown>,
            userSignupFields,
          ),
        adapters: {
          authRepository: {
            async createUserWithIdentity({
              providerId,
              serializedProviderData,
              userFields,
            }) {
              const user = await createUser(
                providerId,
                serializedProviderData,
                // Using any here because we want to avoid TypeScript errors and
                // rely on Prisma to validate the data.
                userFields as any,
              )
              if (user.auth === null) {
                throw new Error('Auth entity not found after username signup')
              }
              return { authId: user.auth.id, user }
            },
          },
          hooks: {
            onBeforeSignup: ({ request, providerId }) =>
              onBeforeSignupHook({ req: request, providerId }),
            onAfterSignup: ({ request, providerId, user }) =>
              onAfterSignupHook({ req: request, providerId, user }),
          },
        },
      })
    } catch (e: unknown) {
      rethrowPossibleAuthServiceError(e)
      rethrowPossibleAuthError(e)
    }

    res.json({ success: true })
  })
}
