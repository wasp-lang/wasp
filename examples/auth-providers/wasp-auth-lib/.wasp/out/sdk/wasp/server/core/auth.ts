import { getSessionAndUserFromBearerToken } from '../auth/session.js'
import { createInvalidCredentialsError } from '../auth/utils.js'
import { defineHandler } from '../utils.js'
import { HttpError } from '../HttpError.js'

/**
 * Auth middleware
 *
 * If the request includes an `Authorization` header it will try to authenticate the request,
 * otherwise it will let the request through.
 *
 * - If authentication succeeds it sets `req.sessionId` and `req.user`
 *   - `req.user` is the user that made the request and it's used in
 *      all Wasp features that need to know the user that made the request.
 *   - `req.sessionId` is the ID of the session that authenticated the request.
 * - If the request is not authenticated, it throws an error.
 */
const auth = defineHandler(async (req, res, next) => {
  const authHeader = req.get('Authorization')
  // NOTE(matija): for now we let tokenless requests through and make it operation's
  // responsibility to verify whether the request is authenticated or not. In the future
  // we will develop our own system at Wasp-level for that.
  if (!authHeader) {
    req.sessionId = null
    req.user = null
    return next()
  }

  const sessionAndUser = await getSessionAndUserFromBearerToken(req)

  if (sessionAndUser === null) {
    throw createInvalidCredentialsError()
  }

  req.sessionId = sessionAndUser.sessionId
  req.user = sessionAndUser.user

  next()
})

export default auth

/**
 * Middleware factory for provider-restricted operations and APIs
 * (`auth: ["wasp", ...]`). Unlike plain `auth: true` (which attaches the user
 * and leaves the check to the operation), the restricted form is
 * self-enforcing: naming providers means "require a session from one of
 * these", so Wasp gates it. No session is a 401 (go log in); a valid session
 * from a non-listed provider is a 403 (logged in, but not like this) -- the
 * distinction that keeps clients from redirecting an already-logged-in user
 * back to the login page. A pure comparison against the provider recorded on
 * the session at mint time; no provider code runs.
 */
export function requireSessionProvider(requiredProviderIds: string[]) {
  return defineHandler(async (req, _res, next) => {
    if (req.user == null) {
      throw createInvalidCredentialsError()
    }
    if (!requiredProviderIds.includes(req.user.sessionProviderId)) {
      throw new HttpError(
        403,
        `Authenticated via '${req.user.sessionProviderId}', but this requires signing in via one of: ${requiredProviderIds.join(', ')}.`,
      )
    }
    next()
  })
}
