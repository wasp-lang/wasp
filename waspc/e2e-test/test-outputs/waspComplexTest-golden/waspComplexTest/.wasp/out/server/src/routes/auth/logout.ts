import { invalidateSession } from 'wasp/auth/session'
import { createInvalidCredentialsError } from 'wasp/auth/utils'
import { defineHandler } from 'wasp/server/utils'

export default defineHandler(async (req, res) => {
  if (req.sessionId) {
    await invalidateSession(req.sessionId)
    return res.json({ success: true })
  } else {
    throw createInvalidCredentialsError()
  }
})
