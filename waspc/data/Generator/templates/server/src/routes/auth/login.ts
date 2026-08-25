import { defineHandler } from 'wasp/server/utils'
import { createInvalidCredentialsError } from 'wasp/server/auth/utils'
import { exchangeRequestForSession } from 'wasp/server/auth/session'

/**
 * The credential exchange: the client sends the auth provider's credential as
 * `Authorization: Bearer <credential>`, the provider verifies it, and Wasp
 * mints the first-party session every subsequent request authenticates with.
 * This is the external-provider analogue of Wasp auth's own login routes.
 */
export default defineHandler(async (req, res) => {
  const session = await exchangeRequestForSession(req)
  if (session === null) {
    throw createInvalidCredentialsError()
  }
  res.json({ sessionId: session.id })
})
