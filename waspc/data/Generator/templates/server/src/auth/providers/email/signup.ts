import { Request, Response } from 'express'
import { signupWithEmail } from '@wasp.sh/lib-auth/node'
import type { UserSignupFields } from 'wasp/auth/providers/types'
import {
  createUser,
  deleteUserByAuthId,
  doFakeWork,
  findAuthIdentity,
  rethrowPossibleAuthError,
  rethrowPossibleAuthServiceError,
  type CreateUserResult,
  validateAndGetUserFields,
} from 'wasp/auth/utils'
import { GetVerificationEmailContentFn } from 'wasp/server/auth/email'
import {
  createEmailVerificationLink,
  sendEmailVerificationEmail,
} from 'wasp/server/auth/email/utils'
import { EmailFromField } from 'wasp/server/email/core/types'
import { onAfterSignupHook, onBeforeSignupHook } from '../../hooks.js'

export function getSignupRoute({
  userSignupFields,
  fromField,
  clientRoute,
  getVerificationEmailContent,
  isEmailAutoVerified,
}: {
  userSignupFields?: UserSignupFields
  fromField: EmailFromField
  clientRoute: string
  getVerificationEmailContent: GetVerificationEmailContentFn
  isEmailAutoVerified: boolean
}) {
  return async function signup(
    req: Request<{ email: string; password: string }>,
    res: Response,
  ): Promise<void> {
    const fields = req.body

    try {
      await signupWithEmail<typeof req, CreateUserResult, Record<string, any>>({
        fields,
        request: req,
        getUserFields: (fields) =>
          validateAndGetUserFields(
            fields as Record<string, unknown>,
            userSignupFields,
          ),
        isEmailAutoVerified,
        adapters: {
          authRepository: {
            async findIdentity(providerId) {
              const authIdentity = await findAuthIdentity(providerId)
              return authIdentity === null ? null : {
                authId: authIdentity.authId,
                providerName: 'email',
                providerUserId: authIdentity.providerUserId,
                providerData: authIdentity.providerData,
              }
            },
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
                throw new Error('Auth entity not found after email signup')
              }
              return { authId: user.auth.id, user }
            },
            deleteUserByAuthId,
          },
          hooks: {
            onBeforeSignup: ({ request, providerId }) =>
              onBeforeSignupHook({ req: request, providerId }),
            onAfterSignup: ({ request, providerId, user }) =>
              onAfterSignupHook({ req: request, providerId, user }),
          },
          emailVerification: {
            createVerificationLink: (email) =>
              createEmailVerificationLink(email, clientRoute),
            sendVerificationEmail: ({ email, verificationLink }) =>
              sendEmailVerificationEmail(email, {
                from: fromField,
                to: email,
                ...getVerificationEmailContent({ verificationLink }),
              }),
          },
          clock: {
            now: () => new Date(),
          },
          workSimulator: {
            doFakeWork,
          },
        },
      })
    } catch (e: unknown) {
      rethrowPossibleAuthServiceError(e)
      rethrowPossibleAuthError(e)
    }

    res.json({ success: true })
  }
}
