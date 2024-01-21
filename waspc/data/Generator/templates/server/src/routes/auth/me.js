import { serialize as superjsonSerialize } from 'superjson'
import { handleRejection } from 'wasp/server/utils'
import { throwInvalidCredentialsError } from '../../auth/utils.js'

export default handleRejection(async (req, res) => {
  if (req.user) {
    return res.json(superjsonSerialize(req.user))
  } else {
    throwInvalidCredentialsError()
  }
})
