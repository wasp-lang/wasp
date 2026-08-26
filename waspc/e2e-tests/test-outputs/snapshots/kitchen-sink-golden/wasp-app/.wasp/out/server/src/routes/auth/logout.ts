import { defineHandler } from 'wasp/server/utils'
import { invalidateSession } from 'wasp/server/auth/session'
import { appDelivery } from 'wasp/server/core/delivery'

export default defineHandler(async (req, res) => {
  const sessionId = appDelivery.readHttpSessionCredential(req)
  if (sessionId) {
    await invalidateSession(sessionId)
  }

  appDelivery.clearSessionCredential(res)
  res.json({ success: true })
})
