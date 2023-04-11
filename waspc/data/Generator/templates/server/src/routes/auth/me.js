{{={= =}=}}
import { serialize as superjsonSerialize } from 'superjson'
import { handleRejection } from '../../utils.js'
import { throwInvalidCredentialsError } from '../../auth/utils.js'

export default handleRejection(async (req, res) => {
  if (req.{= userEntityLower =}) {
    return res.json(superjsonSerialize(req.{= userEntityLower =}))
  } else {
    throwInvalidCredentialsError()
  }
})
