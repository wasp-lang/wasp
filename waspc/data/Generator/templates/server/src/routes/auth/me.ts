import { serialize } from 'wasp/core/serialization'
import { defineHandler } from 'wasp/server/utils'

export default defineHandler(async (req, res) => {
  if (req.user) {
    res.json(serialize(req.user))
  } else {
    res.json(serialize(null))
  }
})
