{{={= =}=}}
import { handleRejection } from '../../utils.js'

import {= operationName =} from "{= operationImportPath =}"

export default handleRejection(async (req, res) => {
  const args = req.query || {}

  // TODO: Have a flag for if we should autoconvert numbers.
  // Just doing some testing right now.
  if (true) {
    if (req.query.id) {
      req.query.id = parseInt(req.query.id);
    }
  }

  const context = {
    {=# userEntityLower =}
    user: req.user
    {=/ userEntityLower =}
  }

  const result = await {= operationName =}(args, context)
  res.json(result)
})
