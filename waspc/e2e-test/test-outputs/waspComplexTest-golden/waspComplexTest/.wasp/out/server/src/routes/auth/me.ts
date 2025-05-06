import { serialize as superjsonSerialize } from 'superjson'
import { defineHandler } from 'wasp/server/utils'

export default defineHandler(async (req, res) => {
  if (req.user) {
    res.json(superjsonSerialize(req.user))
  } else {
    res.json(superjsonSerialize(null))
  }
})
