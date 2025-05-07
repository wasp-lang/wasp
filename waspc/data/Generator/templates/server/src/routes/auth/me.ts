import { serialize as superjsonSerialize } from 'superjson'
import { defineHandler } from 'wasp/server/utils'

export default defineHandler(async (req, res) => {
  if (req.user) {
    return res.json(superjsonSerialize(req.user))
  } else {
    return res.json(superjsonSerialize(null))
  }
})
