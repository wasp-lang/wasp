{{={= =}=}}
import { handleRejection } from '../../utils.js'

import {= operationName =} from "{= operationImportPath =}"

export default handleRejection(async (req, res) => {
  {=! TODO: When generating express route for query, generated code would be most human-like if we
    generated GET route that uses query arguments.
    However, for that, we need to know the types of the arguments so we can cast/parse them.
    Also, there is limit on URI length, which could be problem if users want to send some bigger
    JSON objects or smth.
    So for now we are just going with POST that has JSON in the body -> generated code is not
    as human-like as it should be though. =}
  const args = req.body || {}

  const context = {
    {=# userEntityLower =}
    user: req.user
    {=/ userEntityLower =}
  }

  const result = await {= operationName =}(args, context)
  res.json(result)
})
