{{={= =}=}}
import { handleRejection } from '../../../utils.js'
import { createUser } from '../../utils.js'
import { ensureValidUsernameSignupArgs } from '../../validation.js'
import { validateAndGetAdditionalFields } from '../../utils.js'

export default handleRejection(async (req, res) => {
  const userFields = req.body || {}
  ensureValidUsernameSignupArgs(userFields)

  const additionalFields = await validateAndGetAdditionalFields(userFields)

  await createUser({
    ...additionalFields,
    username: userFields.username,
    password: userFields.password,
  })

  return res.json({ success: true })
})
