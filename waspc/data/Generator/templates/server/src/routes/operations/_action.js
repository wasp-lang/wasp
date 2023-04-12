{{={= =}=}}
import { 
  deserialize as superjsonDeserialize,
  serialize as superjsonSerialize,
} from 'superjson'
import { handleRejection } from '../../utils.js'
{=& operationImportStmt =}

export default handleRejection(async (req, res) => {
  const args = (req.body && superjsonDeserialize(req.body)) || {}

  const context = {
    {=# userEntityLower =}
    user: req.user
    {=/ userEntityLower =}
  }

  const result = await {= operationName =}(args, context)
  const serializedResult = superjsonSerialize(result)
  res.json(serializedResult)
})

