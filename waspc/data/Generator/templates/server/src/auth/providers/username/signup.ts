{{={= =}=}}
import { handleRejection } from 'wasp/server/utils'
import {
  createProviderId,
  createUser,
  rethrowPossibleAuthError,
  sanitizeAndSerializeProviderData,
} from '../../utils.js'
import {
  ensureValidUsername,
  ensurePasswordIsPresent,
  ensureValidPassword,
} from '../../validation.js'
import { validateAndGetUserFields } from '../../utils.js'
import { type UserSignupFields } from '../types.js'

export function getSignupRoute({
  userSignupFields,
}: {
  userSignupFields?: UserSignupFields;
}) {
  return handleRejection(async function signup(req, res) {
    const fields = req.body ?? {}
    ensureValidArgs(fields)
  
    const userFields = await validateAndGetUserFields(
      fields,
      userSignupFields,
    );
  
    const providerId = createProviderId('username', fields.username)
    const providerData = await sanitizeAndSerializeProviderData<'username'>({
      hashedPassword: fields.password,
    })
  
    try {
      await createUser(
        providerId,
        providerData,
        // Using any here because we want to avoid TypeScript errors and
        // rely on Prisma to validate the data.
        userFields as any
      )
    } catch (e: unknown) {
      rethrowPossibleAuthError(e)
    }
  
    return res.json({ success: true })
  })
}

function ensureValidArgs(args: unknown): void {
  ensureValidUsername(args)
  ensurePasswordIsPresent(args)
  ensureValidPassword(args)
}
