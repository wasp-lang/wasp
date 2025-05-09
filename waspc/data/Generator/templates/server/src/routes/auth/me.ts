import { serialize } from 'wasp/core/serialization'
import { defineHandler } from 'wasp/server/utils'

export default defineHandler(async (req, res) => {
  if (req.user) {
    return res.json(serialize(req.user))
  } else {
    return res.json(serialize(null))
  }
})
