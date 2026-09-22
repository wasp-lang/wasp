import { defineHandler } from 'wasp/server/utils'
import { createInvalidCredentialsError } from 'wasp/server/auth/utils'
import { authSchemes } from 'wasp/server/auth/schemes'
import { sendWebResponse, toWebRequest } from 'wasp/server/auth/http'

/**
 * Sign out: the scheme that authenticated this request invalidates the
 * credential it carries and says how (an expired cookie, a plain 200). A
 * handler without `signOut` has nothing server-side to end; the client
 * drops its credential regardless.
 */
export default defineHandler(async (req, res) => {
  if (req.user == null || req.authScheme == null) {
    throw createInvalidCredentialsError()
  }
  const handler = authSchemes[req.authScheme as keyof typeof authSchemes]
  const response = (await handler.signOut?.(toWebRequest(req))) ?? Response.json({ success: true })
  await sendWebResponse(res, response)
})
