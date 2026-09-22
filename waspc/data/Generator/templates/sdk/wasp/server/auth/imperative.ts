{{={= =}=}}
import type { Request as ExpressRequest, Response as ExpressResponse } from 'express'
import type { AuthSchemeName } from '../../auth/scheme.js'
import type { AuthUserData } from '../../auth/user.js'
import type { SignInProperties } from './handler/types.js'
import { prisma } from '../index.js'
import { sendAuthResponse, toWebRequest } from './issuer.js'
import { authSchemes, defaultScheme, issueSignInFor, signOutEverywhereForAuthId } from './schemes.js'
import { authenticateRequest } from './session.js'
import { findAuthWithUserBy } from './utils.js'

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
 * Signs an EXISTING user in and writes the credential to `res`: a body
 * carrying the token for a bearer scheme (the client adopts it with
 * `setCredential`), a `Set-Cookie` header for a cookie scheme. The subject
 * is a user row, never a principal the app made up, so `context.user`
 * stays a real user everywhere.
 *
 * The user is signed in AS `scheme` (default: the app's default scheme),
 * through one of the identities the user holds under it, so a user with no
 * identity in that scheme cannot be signed into it. The app's login hooks
 * fire unless `skipHooks`. A scheme that declares no `credentials` (Clerk)
 * rejects with `wasp-auth/undeclared-facet`.
 */
export async function signIn(
  user: UserRef,
  res: ExpressResponse,
  opts?: { scheme?: AuthSchemeName; properties?: SignInProperties; req?: ExpressRequest; skipHooks?: boolean },
): Promise<void> {
  const scheme = opts?.scheme ?? defaultScheme
  const auth = await findAuthWithUserBy({ userId: user.id })
  if (auth === null) {
    throw new Error(`Cannot sign in: the user has no auth data.`)
  }
  const identity = await prisma.{= authIdentityEntityLower =}.findFirst({
    where: { authId: auth.id, handlerName: scheme },
    orderBy: [{ providerName: 'asc' }, { providerUserId: 'asc' }],
    select: { providerName: true, providerUserId: true },
  })
  if (identity === null) {
    throw new Error(`Cannot sign the user into auth scheme '${scheme}': the user holds no identity in it.`)
  }
  const { response } = await issueSignInFor(
    scheme,
    { ...identity, authId: auth.id },
    { req: opts?.req, properties: opts?.properties, skipHooks: opts?.skipHooks },
  )
  sendAuthResponse(res, response)
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
    sendAuthResponse(res, { status: 200, body: { success: true } })
    return
  }
  const handler = authSchemes[result.scheme]
  const response = (await handler.signOut?.(toWebRequest(req))) ?? { status: 200, body: { success: true } }
  sendAuthResponse(res, response)
}

// PUBLIC API
/**
 * Ends EVERY Wasp-issued credential of the user, in every scheme that issues
 * them ("log out every device"). A handler that owns its credentials (Clerk,
 * Better Auth) is not reached: end those through the handler.
 */
export async function signOutEverywhere(user: UserRef): Promise<void> {
  const auth = await findAuthWithUserBy({ userId: user.id })
  if (auth !== null) {
    await signOutEverywhereForAuthId(auth.id)
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
  const response = (await handler.challenge?.(toWebRequest(req))) ?? { status: 401, body: { message: 'Invalid credentials' } }
  sendAuthResponse(res, response)
}

// PUBLIC API
/** Answers "you may not" the way `scheme` would: a 403 by default. */
export async function forbid(
  req: ExpressRequest,
  res: ExpressResponse,
  opts?: { scheme?: AuthSchemeName },
): Promise<void> {
  const handler = authSchemes[opts?.scheme ?? defaultScheme]
  const response = (await handler.forbid?.(toWebRequest(req))) ?? { status: 403, body: { message: 'Forbidden' } }
  sendAuthResponse(res, response)
}
