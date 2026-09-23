import { defineHandler } from 'wasp/server/utils'
import { createInvalidCredentialsError } from 'wasp/server/auth/utils'
import { signOutScheme } from 'wasp/server/auth/schemes'
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
  await sendWebResponse(res, await signOutScheme(req.authScheme as Parameters<typeof signOutScheme>[0], toWebRequest(req)))
})
