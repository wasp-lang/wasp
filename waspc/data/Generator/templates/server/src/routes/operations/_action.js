{{={= =}=}}
import { handleRejection } from '../../utils.js'
{=& jsFnImportStatement =}

export default handleRejection(async (req, res) => {
  const args = req.body || {}
  const context = {}
  const result = await {= jsFnIdentifier =}(args, context)
  res.json(result)
})

