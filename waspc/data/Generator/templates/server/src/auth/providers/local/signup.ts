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
  const fields = req.body || {}
  ensureValidArgs(fields)

  const additionalFields = await validateAndGetAdditionalFields(fields)

  await createAuthWithUser(
    {
      username: fields.username,
      password: fields.password,
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
