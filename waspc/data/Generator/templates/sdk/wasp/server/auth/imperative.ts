{{={= =}=}}
import type { Request as ExpressRequest, Response as ExpressResponse } from 'express'
import type { AuthSchemeName } from '../../auth/scheme.js'
import type { AuthUserData } from '../../auth/user.js'
import type { AuthHandler, SignInProperties } from './handler/types.js'
import { prisma } from '../index.js'
import { sendWebResponse, toWebRequest } from './http.js'
import { authSchemes, defaultScheme, issueSignInFor, signOutEverywhereForAuthId } from './schemes.js'
import { authenticateRequest } from './session.js'
import { findAuthWithUserBy, type ProviderId } from './utils.js'

/**
 * The imperative auth API: what app code calls to do by hand what the auth
 * middleware and the handlers' routes do for it. A custom signup that signs
 * the new user in, impersonation, "log out every device". Modelled on
 * ASP.NET Core's `HttpContext.AuthenticateAsync / SignInAsync / SignOutAsync
 * / ChallengeAsync / ForbidAsync`.
 *
 * Every function that answers the browser takes `res` and writes to it: a
 * body carrying the token, a `Set-Cookie` header, a redirect. So they are
 * called from `api()` routes, which have `res`.
 */

/** The user a sign-in or sign-out is about: any object carrying the user entity's id. */
type UserRef = Pick<AuthUserData, 'id'>

// PUBLIC API
/**
 * Who sent this request, without requiring anyone: null for an anonymous
 * request. For routes that are public but behave differently for a
 * signed-in user. `scheme` defaults to the app's default scheme.
 */
export async function authenticate(
  req: ExpressRequest,
  opts?: { scheme?: AuthSchemeName },
): Promise<AuthUserData | null> {
  const result = await authenticateRequest(req, [opts?.scheme ?? defaultScheme])
  return result === null ? null : result.user
}

// PUBLIC API
/**
 * Signs ONE identity in, named exactly by its key (`handlerName`,
 * `providerName`, `providerUserId`), and writes the credential to `res`: a
 * body carrying the token for a bearer scheme (the client adopts it with
 * `setCredential`), a `Set-Cookie` header for a cookie scheme. Nothing is
 * guessed: the scheme is the identity's handler, the user is the identity's
 * account, and the app's login hooks receive that identity. A scheme that
 * declares no `credentials` (Clerk) rejects with `wasp-auth/undeclared-facet`.
 */
export async function signIn(
  identity: ProviderId,
  res: ExpressResponse,
  opts?: { properties?: SignInProperties; skipHooks?: boolean },
): Promise<void> {
  const scheme = identity.handlerName
  if (!(scheme in authSchemes)) {
    throw new Error(`Cannot sign in: '${scheme}' is not one of the app's auth schemes.`)
  }
  const row = await prisma.{= authIdentityEntityLower =}.findUnique({
    where: {
      handlerName_providerName_providerUserId: {
        handlerName: identity.handlerName,
        providerName: identity.providerName,
        providerUserId: identity.providerUserId,
      },
    },
    select: { authId: true },
  })
  if (row === null) {
    throw new Error(`Cannot sign in: no identity '${identity.providerUserId}' under provider '${identity.providerName}' of auth scheme '${scheme}'.`)
  }
  const { response } = await issueSignInFor(
    scheme as AuthSchemeName,
    { providerName: identity.providerName, providerUserId: identity.providerUserId, authId: row.authId },
    { properties: opts?.properties, skipHooks: opts?.skipHooks },
  )
  await sendWebResponse(res, response)
}

// PUBLIC API
/**
 * Ends the credential `req` carries, through the scheme that authenticated
 * it, and writes the answer to `res` (an expired cookie, a plain 200). Does
 * nothing for an anonymous request.
 */
export async function signOut(req: ExpressRequest, res: ExpressResponse): Promise<void> {
  const result = await authenticateRequest(req, Object.keys(authSchemes) as AuthSchemeName[])
  if (result === null) {
    await sendWebResponse(res, Response.json({ success: true }))
    return
  }
  const handler = authSchemes[result.scheme]
  const response = (await handler.signOut?.(toWebRequest(req))) ?? Response.json({ success: true })
  await sendWebResponse(res, response)
}

// PUBLIC API
/**
 * Ends EVERY credential of the user ("log out every device"): the Wasp-issued
 * ones in every scheme that issues them, and the handler-owned ones of every
 * handler that implements `signOutEverywhere` (Better Auth's sessions,
 * Clerk's). Throws when no scheme could act for this user, rather than
 * ending nothing in silence.
 */
export async function signOutEverywhere(user: UserRef): Promise<void> {
  const auth = await findAuthWithUserBy({ userId: user.id })
  if (auth === null) {
    throw new Error(`Cannot sign out everywhere: the user has no auth data.`)
  }
  let acted = await signOutEverywhereForAuthId(auth.id)
  const identities = await prisma.{= authIdentityEntityLower =}.findMany({
    where: { authId: auth.id },
    select: { handlerName: true, providerName: true, providerUserId: true },
  })
  for (const identity of identities) {
    const handler = (authSchemes as Record<string, AuthHandler>)[identity.handlerName]
    if (handler?.signOutEverywhere !== undefined) {
      await handler.signOutEverywhere({ providerName: identity.providerName, providerUserId: identity.providerUserId })
      acted = true
    }
  }
  if (!acted) {
    throw new Error(
      `Cannot sign out everywhere: no auth scheme issues credentials for this user, and none of the user's handlers implements signOutEverywhere.`,
    )
  }
}

// PUBLIC API
/**
 * Answers "log in first" the way `scheme` would: a 401, or a redirect to the
 * login page for a cookie scheme.
 */
export async function challenge(
  req: ExpressRequest,
  res: ExpressResponse,
  opts?: { scheme?: AuthSchemeName },
): Promise<void> {
  const handler = authSchemes[opts?.scheme ?? defaultScheme]
  const response = (await handler.challenge?.(toWebRequest(req))) ?? Response.json({ message: 'Invalid credentials' }, { status: 401 })
  await sendWebResponse(res, response)
}

// PUBLIC API
/** Answers "you may not" the way `scheme` would: a 403 by default. */
export async function forbid(
  req: ExpressRequest,
  res: ExpressResponse,
  opts?: { scheme?: AuthSchemeName },
): Promise<void> {
  const handler = authSchemes[opts?.scheme ?? defaultScheme]
  const response = (await handler.forbid?.(toWebRequest(req))) ?? Response.json({ message: 'Forbidden' }, { status: 403 })
  await sendWebResponse(res, response)
}
