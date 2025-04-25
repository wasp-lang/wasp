import { serialize } from 'wasp/core/serialization'
import { handleRejection } from 'wasp/server/utils'
import { createInvalidCredentialsError } from 'wasp/auth/utils'

export default handleRejection(async (req, res) => {
  if (req.user) {
    return res.json(serialize(req.user))
  } else {
    throw createInvalidCredentialsError()
  }
})
