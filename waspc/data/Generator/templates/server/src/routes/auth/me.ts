import { serialize as superjsonSerialize } from 'superjson'
import { createInvalidCredentialsError } from 'wasp/auth/utils'
import { defineHandler } from 'wasp/server/utils'

export default defineHandler(async (req, res) => {
  if (req.user) {
    return res.json(superjsonSerialize(req.user))
  } else {
    throw createInvalidCredentialsError()
  }
})
