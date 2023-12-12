{{={= =}=}}
import { handleRejection } from '../../../utils.js'
import { createAuthWithUser } from '../../utils.js'
import {
  ensureValidUsername,
  ensurePasswordIsPresent,
  ensureValidPassword,
} from '../../validation.js'
import { validateAndGetAdditionalFields } from '../../utils.js'
import { hashPassword } from '../../../core/auth.js'

export default handleRejection(async (req, res) => {
  const fields = req.body || {}
  ensureValidArgs(fields)

  const additionalFields = await validateAndGetAdditionalFields(fields)

  // TODO: create auth identity with username (providerName="username" and providerUserId=username)
  // TODO: set the hashed password in the JSON field "providerData"
  const password = await hashPassword(fields.password)
  await createAuthWithUser(
    {
      identities: {
        create: {
          providerName: 'username',
          providerUserId: fields.username,
          providerData: JSON.stringify({
            password,
          }),
        },
      },
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
