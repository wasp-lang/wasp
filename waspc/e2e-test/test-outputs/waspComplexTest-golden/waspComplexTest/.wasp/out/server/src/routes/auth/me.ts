import { serialize as superjsonSerialize } from 'superjson'
import { handleRejection } from 'wasp/server/utils'
import { createInvalidCredentialsError } from 'wasp/auth/utils'

export default handleRejection(async (req, res) => {
  if (req.user) {
    return res.json(superjsonSerialize(req.user))
  } else {
    throw createInvalidCredentialsError()
  }
})
