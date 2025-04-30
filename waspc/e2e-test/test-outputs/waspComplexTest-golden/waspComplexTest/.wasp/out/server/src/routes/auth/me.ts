import { serialize } from 'wasp/core/serialization'
import { handleRejection } from 'wasp/server/utils'

export default handleRejection(async (req, res) => {
  if (req.user) {
    return res.json(serialize(req.user))
  } else {
    return res.json(superjsonSerialize(null))
  }
})
