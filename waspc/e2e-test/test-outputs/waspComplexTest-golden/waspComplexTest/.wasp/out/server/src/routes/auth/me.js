import { serialize as superjsonSerialize } from 'superjson'
import { handleRejection } from '../../utils.js'

export default handleRejection(async (req, res) => {
  if (req.user) {
    return res.json(superjsonSerialize(req.user))
  } else {
    return res.status(401).send()
  }
})
