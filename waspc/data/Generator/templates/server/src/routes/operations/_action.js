{{={= =}=}}
import { handleRejection } from '../../utils.js'

{=& operationImportStmt =}

export default handleRejection(async (req, res) => {
  const args = req.body || {}

  const context = {
    {=# userEntityLower =}
    user: req.user
    {=/ userEntityLower =}
  }
  const result = await {= operationName =}(args, context)
  res.json(result)
})

