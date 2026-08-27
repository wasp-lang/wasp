import { Request, Response } from 'express'
import type { UserSignupFields } from 'wasp/auth/providers/types'
import {
  createProviderId,
  doFakeWork,
  rethrowPossibleAuthError,
  validateAndGetUserFields,
} from 'wasp/server/auth/utils'
import { getIdentityStore } from 'wasp/server/auth/identityStore'
import { hashPassword } from 'wasp/server/auth/password'
import {
  ensurePasswordIsPresent,
  ensureValidEmail,
  ensureValidPassword,
} from 'wasp/auth/validation'
import { HttpError } from 'wasp/server'
import { GetVerificationEmailContentFn } from 'wasp/server/auth/email'
import {
  createEmailVerificationLink,
  isEmailResendAllowed,
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
    ensureValidArgs(fields)

    const emailIdentities = getIdentityStore('email')
    const providerId = createProviderId('email', fields.email)
    const existingIdentity = await emailIdentities.find(fields.email)

    /**
     *
     * There are two cases to consider in the case of an existing user:
     * - if we allow unverified login
     * - if the user is already verified
     *
     * Let's see what happens when we **don't** allow unverified login:
     *
     * We are handling the case of an existing auth identity in two ways:
     *
     * 1. If the user already exists and is verified, we don't want
     *   to leak that piece of info and instead we pretend that the user
     *   was created successfully.
     *    - This prevents the attacker from learning which emails already have
     *        an account created.
     *
     * 2. If the user is not verified:
     *   - We check when we last sent a verification email and if it was less than X seconds ago,
     *     we don't send another one.
     *   - If it was more than X seconds ago, we delete the user and create a new one.
     *   - This prevents the attacker from creating an account with somebody
     *     else's email address and therefore permanently making that email
     *     address unavailable for later account creation (by real owner).
     */
    if (existingIdentity) {

      // TOOD: faking work makes sense if the time spent on faking the work matches the time
      // it would take to send the email. Atm, the fake work takes obviously longer than sending
      // the email!
      if (existingIdentity.data.isEmailVerified) {
        await doFakeWork()
        res.json({ success: true })
        return
      }

      // TODO: we are still leaking information here since when we are faking work
      // we are not checking if the email was sent or not!
      const { isResendAllowed, timeLeft } = isEmailResendAllowed(
        existingIdentity.data,
        'emailVerificationSentAt',
      )
      if (!isResendAllowed) {
        throw new HttpError(
          400,
          `Please wait ${timeLeft} secs before trying again.`,
        )
      }

      try {
        await emailIdentities.deleteUser(fields.email)
      } catch (e: unknown) {
        rethrowPossibleAuthError(e)
      }
    }

    // The hook runs first so it can veto the signup (by throwing) before the
    // developer's `userSignupFields` getters run.
    try {
      await onBeforeSignupHook({ req, providerId })
    } catch (e: unknown) {
      rethrowPossibleAuthError(e)
    }

    const userFields = await validateAndGetUserFields(fields, userSignupFields)

    try {
      const user = await emailIdentities.createIdentity(
        fields.email,
        {
          data: {
            isEmailVerified: isEmailAutoVerified ? true : false,
            emailVerificationSentAt: null,
            passwordResetSentAt: null,
          },
          // Hashing is the flow's explicit job -- storage never hashes.
          secrets: {
            hashedPassword: await hashPassword(fields.password),
          },
        },
        // Using any here because we want to avoid TypeScript errors and
        // rely on Prisma to validate the data.
        userFields as any,
      )
      await onAfterSignupHook({ req, providerId, user })
    } catch (e: unknown) {
      rethrowPossibleAuthError(e)
    }

    // Wasp allows for auto-verification of emails in development mode to
    // make writing e2e tests easier.
    if (isEmailAutoVerified) {
      res.json({ success: true })
      return
    }

    const verificationLink = await createEmailVerificationLink(
      fields.email,
      clientRoute,
    )
    try {
      await sendEmailVerificationEmail(fields.email, {
        from: fromField,
        to: fields.email,
        ...getVerificationEmailContent({ verificationLink }),
      })
    } catch (e: unknown) {
      console.error('Failed to send email verification email:', e)
      throw new HttpError(500, 'Failed to send email verification email.')
    }

    res.json({ success: true })
  }
}

function ensureValidArgs(args: object): void {
  ensureValidEmail(args)
  ensurePasswordIsPresent(args)
  ensureValidPassword(args)
}
