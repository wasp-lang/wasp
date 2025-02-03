{{={= =}=}}
import { handleRejection } from 'wasp/server/utils'
{=# isCookieAuthEnabled =}
import { 
  getSessionAndUserFromSessionId,
  getSessionFromCookie,
  createSessionCookieWithHttpOnly,
  createBlankCookie
} from 'wasp/auth/session'
{=/ isCookieAuthEnabled =}
{=^ isCookieAuthEnabled =}
import { getSessionAndUserFromBearerToken } from 'wasp/auth/session'
{=/ isCookieAuthEnabled =}
import { createInvalidCredentialsError } from 'wasp/auth/utils'

/**
 * Auth middleware
 * 
 * Validates the session cookie or JWT and authenticates the request.
 * - If authentication succeeds it sets `req.sessionId` and `req.user`
 *   - `req.user` is the user that made the request and it's used in
 *      all Wasp features that need to know the user that made the request.
 *   - `req.sessionId` is the ID of the session that authenticated the request.
 * - If the request is not authenticated, it throws an error.
 * 
 * cookieEnabled: true (session cookie) -
 * We can immediately jump in and validate the session cookie.
 * 
 * cookieEnabled: false (JWT local storage) -
 * If the request includes an `Authorization` header it will try to authenticate the request,
 * otherwise it will let the request through.
 *
 */
const auth = handleRejection(async (req, res, next) => {
  {=# isCookieAuthEnabled =}
  const cookieHeader = req.headers.cookie ?? ''
  if (!cookieHeader) {
    req.sessionId = null
    req.user = null
    return next()
  }

  const sessionId = getSessionFromCookie(cookieHeader)
  if (!sessionId) {
    req.sessionId = null
    req.user = null
    return next()
  }
  try {
    const sessionAndUser = await getSessionAndUserFromSessionId(sessionId)

    if (sessionAndUser === null) {
      res.setHeader("Set-Cookie", createBlankCookie().serialize())
      req.sessionId = null
      req.user = null
      return next()
    }

    if (sessionAndUser.session && sessionAndUser.session.fresh) {
      res.setHeader("Set-Cookie", createSessionCookieWithHttpOnly(sessionAndUser.session.id).serialize())
    }

    req.sessionId = sessionAndUser.session.id
    req.user = sessionAndUser.user

    next()
  } catch (err) {
    res.setHeader("Set-Cookie", createBlankCookie().serialize())
    req.sessionId = null
    req.user = null
    return next()
  }
  {=/ isCookieAuthEnabled =}
  {=^ isCookieAuthEnabled =}
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

  req.sessionId = sessionAndUser.session.id
  req.user = sessionAndUser.user

  next()
  {=/ isCookieAuthEnabled =}
})

export default auth
