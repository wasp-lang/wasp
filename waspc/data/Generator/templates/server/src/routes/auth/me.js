{{={= =}=}}
import { handleRejection } from '../../utils.js'

export default handleRejection(async (req, res) => {
  console.log(req)
  if (req.{= userEntityLower =}) {
    return res.json(req.{= userEntityLower =})
  } else {
    return res.status(403).send()
  }
})
