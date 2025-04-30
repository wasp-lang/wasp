import { serialize } from 'wasp/core/serialization'

export default defineHandler(async (req, res) => {
  if (req.user) {
    return res.json(serialize(req.user))
  } else {
    return res.json(superjsonSerialize(null))
  }
})
