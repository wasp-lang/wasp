import type { Request as ExpressRequest, Response as ExpressResponse } from 'express'
import { authenticateRequest } from '../auth/session.js'
import { authSchemes, defaultScheme } from '../auth/schemes.js'
import { sendAuthResponse, toWebRequest } from '../auth/issuer.js'
import { createInvalidCredentialsError } from '../auth/utils.js'
import { defineHandler } from '../utils.js'
import type { AuthSchemeName } from '../../auth/scheme.js'

/**
 * Auth middleware.
 *
 * Authenticates the request with the app's default scheme. An
 * unauthenticated request is let through with `req.user = null`; it is the
 * asset's job to decide whether it needs a user (plain `auth: true`
 * operations check for one). A request that carries a credential the scheme
 * rejects is a 401: a stale or forged credential is never silently
 * downgraded to "anonymous".
 *
 * - `req.user` is the user that made the request; every Wasp feature that
 *   needs the current user reads it.
 * - `req.authScheme` is the scheme that authenticated it.
 * - `req.sessionId` is the scheme's id for the credential, when it has one.
 */
const auth = defineHandler(async (req, res, next) => {
  const result = await authenticateRequest(req, [defaultScheme])
  if (result === null) {
    req.sessionId = null
    req.user = null
    req.authScheme = null
    if (carriesCredential(req)) {
      throw createInvalidCredentialsError()
    }
    return next()
  }
  req.sessionId = result.credentialId ?? null
  req.user = result.user
  req.authScheme = result.scheme
  next()
})

export default auth

// A request that sent something authentication-shaped and still failed is a
// 401, not an anonymous request: `Authorization` for bearer schemes, a
// cookie for cookie schemes. Requests with neither are anonymous.
function carriesCredential(req: ExpressRequest): boolean {
  return req.get('Authorization') !== undefined || req.get('Cookie') !== undefined
}

/**
 * Middleware factory for scheme-restricted assets (`auth: ["wasp", ...]`).
 * The listed schemes are tried in order and the first that authenticates
 * wins. No credential is answered by the first scheme's `challenge` (a 401,
 * or a redirect for a cookie scheme); a request authenticated by a scheme
 * outside the list is answered by that scheme's `forbid` (a 403) -- the
 * distinction that keeps clients from redirecting an already-logged-in user
 * back to the login page.
 */
export function requireSchemes(schemeNames: AuthSchemeName[]) {
  return defineHandler(async (req, res, next) => {
    const result = await authenticateRequest(req, schemeNames)
    if (result !== null) {
      req.sessionId = result.credentialId ?? null
      req.user = result.user
      req.authScheme = result.scheme
      return next()
    }
    const webRequest = toWebRequest(req)
    // Logged in, but not like this: forbid, from whichever scheme knows the user.
    if (req.user != null && req.authScheme !== null && req.authScheme !== undefined) {
      const forbidder = authSchemes[req.authScheme as AuthSchemeName]
      const response = (await forbidder.forbid?.(webRequest)) ?? {
        status: 403,
        body: {
          message: `Authenticated via '${req.authScheme}', but this requires signing in via one of: ${schemeNames.join(', ')}.`,
        },
      }
      return sendAuthResponse(res as ExpressResponse, response)
    }
    const challenger = authSchemes[schemeNames[0]]
    const response = (await challenger.challenge?.(webRequest)) ?? { status: 401, body: { message: 'Invalid credentials' } }
    return sendAuthResponse(res as ExpressResponse, response)
  })
}
