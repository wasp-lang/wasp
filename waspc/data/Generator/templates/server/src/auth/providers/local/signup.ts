{{={= =}=}}
import { handleRejection } from '../../../utils.js'
import {
  createUser,
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

  const providerData = await sanitizeAndSerializeProviderData<'username'>({
    password: fields.password,
  })
  await createUser(
    'username',
    fields.username,
    providerData,
    // Using any here because we want to avoid TypeScript errors and
    // rely on Prisma to validate the data.
    userFields as any
  )

  return res.json({ success: true })
})

function ensureValidArgs(args: unknown): void {
  ensureValidUsername(args)
  ensurePasswordIsPresent(args)
  ensureValidPassword(args)
}
