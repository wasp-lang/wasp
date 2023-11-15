{{={= =}=}}
import { handleRejection } from '../../../utils.js'
import { createUser } from '../../utils.js'
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

  await createUser(
    {
      username: userFields.username,
      password: userFields.password,
    },
    additionalFields
  )

  return res.json({ success: true })
})

function ensureValidArgs(args: unknown): void {
  ensureValidUsername(args)
  ensurePasswordIsPresent(args)
  ensureValidPassword(args)
}
