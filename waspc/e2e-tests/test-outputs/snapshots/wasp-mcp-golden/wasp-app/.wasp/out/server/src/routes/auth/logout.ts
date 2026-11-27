import { defineHandler } from 'wasp/server/utils'
import { createInvalidCredentialsError } from 'wasp/server/auth/utils'
import { invalidateSession } from 'wasp/server/auth/session'

export default defineHandler(async (req, res) => {
  if (req.sessionId) {
    await invalidateSession(req.sessionId)
    res.json({ success: true })
  } else {
    throw createInvalidCredentialsError()
  }
})
