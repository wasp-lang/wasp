import { handleRejection } from 'wasp/server/utils'
import { throwInvalidCredentialsError } from 'wasp/auth/utils'
import { invalidateSession } from 'wasp/auth/session'

export default handleRejection(async (req, res) => {
  if (req.sessionId) {
    await invalidateSession(req.sessionId)
    return res.json({ success: true })
  } else {
    throwInvalidCredentialsError()
  }
})
