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
import { validateAndGetAdditionalFields } from '../../utils.js'

export default handleRejection(async (req, res) => {
  const fields = req.body ?? {}
  ensureValidArgs(fields)

  const userFields = await validateAndGetAdditionalFields(fields)

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

function ensureValidArgs(args: unknown): void {
  ensureValidUsername(args)
  ensurePasswordIsPresent(args)
  ensureValidPassword(args)
}
