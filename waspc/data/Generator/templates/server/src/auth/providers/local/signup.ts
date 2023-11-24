{{={= =}=}}
import { handleRejection } from '../../../utils.js'
import { createAuthWithUser } from '../../utils.js'
import {
  ensureValidUsername,
  ensurePasswordIsPresent,
  ensureValidPassword,
} from '../../validation.js'
import { validateAndGetAdditionalFields } from '../../utils.js'

export default handleRejection(async (req, res) => {
  const userFields = req.body || {}
  ensureValidArgs(userFields)

  const additionalFields = await validateAndGetAdditionalFields(userFields)

  await createAuthWithUser(
    {
      username: userFields.username,
      password: userFields.password,
    },
    // Using any here because we want to avoid TypeScript errors and
    // rely on Prisma to validate the data.
    additionalFields as any
  )

  return res.json({ success: true })
})

function ensureValidArgs(args: unknown): void {
  ensureValidUsername(args)
  ensurePasswordIsPresent(args)
  ensureValidPassword(args)
}
