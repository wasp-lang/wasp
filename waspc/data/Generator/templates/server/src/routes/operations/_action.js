{{={= =}=}}
import { handleRejection } from '../../utils.js'

import {= operationName =} from "{= operationImportPath =}"


export default handleRejection(async (req, res) => {
  const args = req.body || {}
  {=# userEntityLower =}
  const context = { {= userEntityLower =}: req.{= userEntityLower =} }
  {=/ userEntityLower =}
  {=^ userEntityLower =}
  const context = {}
  {=/ userEntityLower =}
  const result = await {= operationName =}(args, context)
  res.json(result)
})

