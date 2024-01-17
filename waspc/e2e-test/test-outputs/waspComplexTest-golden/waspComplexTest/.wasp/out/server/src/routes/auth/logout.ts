import { handleRejection } from '../../utils.js'
import { throwInvalidCredentialsError } from '../../auth/utils.js'
import { invalidateSession } from '../../auth/session.js'

export default handleRejection(async (req, res) => {
  if (req.sessionId) {
    await invalidateSession(req.sessionId)
    return res.json({ success: true })
  } else {
    throwInvalidCredentialsError()
  }
})
