import { defineHandler } from 'wasp/server/utils'
import { HttpError } from 'wasp/server'
import { createInvalidCredentialsError } from 'wasp/server/auth/utils'
import { exchangeRequestForSession } from 'wasp/server/auth/session'
import { authProviders } from 'wasp/server/auth/provider'
import type { AuthProviderId } from 'wasp/auth/provider'

/**
 * The credential exchange: the client POSTs to `/auth/login/:providerId` with
 * the provider's credential as `Authorization: Bearer <credential>`, the
 * *addressed* provider verifies it, and Wasp mints the first-party session
 * every subsequent request authenticates with. Providers that mint sessions
 * from their own routes (Wasp's own auth) simply answer 'unauthenticated'.
 *
 * The addressed provider rejecting the credential is a hard 401 -- the
 * exchange never tries another provider, so which identity a credential
 * becomes can never depend on configuration order.
 */
export default defineHandler(async (req, res) => {
  const providerId = req.params.providerId
  if (!isAuthProviderId(providerId)) {
    throw new HttpError(404, `Unknown auth provider '${String(providerId ?? '')}'.`)
  }

  const session = await exchangeRequestForSession(providerId, req)
  if (session === null) {
    throw createInvalidCredentialsError()
  }
  res.json({ sessionId: session.id })
})

function isAuthProviderId(
  providerId: string | string[] | undefined,
): providerId is AuthProviderId {
  return typeof providerId === 'string' && providerId in authProviders
}
