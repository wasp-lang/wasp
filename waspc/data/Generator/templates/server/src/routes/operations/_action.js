{{={= =}=}}
import { handleRejection } from '../../utils.js'

import {= operationName =} from "{= operationImportPath =}"


export default handleRejection(async (req, res) => {
  const args = req.body || {}
  const context = {}
  const result = await {= operationName =}(args, context)
  res.json(result)
})

