{{={= =}=}}
import { serialize as superjsonSerialize } from 'superjson'
import { handleRejection } from '../../utils.js'

export default handleRejection(async (req, res) => {
  if (req.{= userEntityLower =}) {
    return res.json(superjsonSerialize(req.{= userEntityLower =}))
  } else {
    return res.status(401).send()
  }
})
