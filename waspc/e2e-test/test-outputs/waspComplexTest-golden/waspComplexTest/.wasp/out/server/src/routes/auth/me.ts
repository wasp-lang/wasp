import { serialize as superjsonSerialize } from 'superjson'
import { handleRejection } from 'wasp/server/utils'

export default handleRejection(async (req, res) => {
  if (req.user) {
    return res.json(superjsonSerialize(req.user))
  } else {
    return res.json(superjsonSerialize(null))
  }
})
