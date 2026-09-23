{{={= =}=}}
import type { Request as ExpressRequest, Response as ExpressResponse } from 'express'
import type { AuthSchemeName } from '../../auth/scheme.js'
import type { AuthUserData } from '../../auth/user.js'
import type { SignInProperties } from './handler/types.js'
import { prisma } from '../index.js'
import { sendWebResponse, toWebRequest } from './http.js'
import {
  authSchemeNames,
  carriesWaspCredential,
  challengeScheme,
  defaultScheme,
  forbidScheme,
  issueSignInFor,
  refreshSignInFor,
  signOutCredentialById,
  signOutEverywhereInScheme,
  signOutScheme,
} from './schemes.js'
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
 * account, and the app's login hooks receive that identity. The credential
 * comes from whatever the scheme's `credentials` name, or from the scheme's
 * own `signIn` when it owns its credential (Better Auth); a scheme that can
 * do neither (Clerk) rejects with `wasp-auth/undeclared-facet`.
 */
export async function signIn(
  identity: ProviderId,
  res: ExpressResponse,
  opts?: { properties?: SignInProperties; skipHooks?: boolean },
): Promise<void> {
  const scheme = identity.handlerName
  if (!authSchemeNames.includes(scheme as AuthSchemeName)) {
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
export async function signOut(req: ExpressRequest, res: ExpressResponse, opts?: { scheme?: AuthSchemeName }): Promise<void> {
  if (opts?.scheme !== undefined) {
    await sendWebResponse(res, await signOutScheme(opts.scheme, toWebRequest(req)))
    return
  }
  const result = await authenticateRequest(req, authSchemeNames)
  if (result === null) {
    await sendWebResponse(res, Response.json({ success: true }))
    return
  }
  await sendWebResponse(res, await signOutScheme(result.scheme, toWebRequest(req)))
}

// PUBLIC API
/**
 * Reissues the credential `req` carries: a new id and issue time, the same
 * account and login scheme, no hooks (it is not a login). What a security
 * change calls so the current device survives a cut-off, and what defeats
 * session fixation after a privilege change. Only for a Wasp-issued
 * credential; throws for an anonymous request or a handler-owned one.
 */
export async function refreshSignIn(req: ExpressRequest, res: ExpressResponse): Promise<void> {
  const request = toWebRequest(req)
  const result = await authenticateRequest(req, authSchemeNames)
  if (result === null) {
    throw new Error('Cannot refresh the sign-in: the request carries no valid credential.')
  }
  const response = await refreshSignInFor(result.scheme, request)
  if (response === null) {
    throw new Error(
      `Cannot refresh the sign-in: the credential was issued by auth scheme '${result.scheme}' itself, not by Wasp, so only that handler can reissue it.`,
    )
  }
  await sendWebResponse(res, response)
}

// PUBLIC API
/**
 * Ends every credential of the caller's account EXCEPT the one `req`
 * carries: the cut-off, then a refresh of the current credential so it is
 * issued after the cut-off. What a password change calls. Same
 * preconditions as `refreshSignIn`, checked before anything is stamped.
 */
export async function signOutOthers(req: ExpressRequest, res: ExpressResponse): Promise<void> {
  const request = toWebRequest(req)
  const result = await authenticateRequest(req, authSchemeNames)
  if (result === null) {
    throw new Error('Cannot sign out other devices: the request carries no valid credential.')
  }
  if (!(await carriesWaspCredential(result.scheme, request))) {
    throw new Error(
      `Cannot sign out other devices: the credential was issued by auth scheme '${result.scheme}' itself, not by Wasp, so Wasp could not keep it alive past the cut-off.`,
    )
  }
  await signOutEverywhere(result.user)
  const response = await refreshSignInFor(result.scheme, request)
  if (response === null) {
    throw new Error('Cannot sign out other devices: the credential could not be reissued.')
  }
  await sendWebResponse(res, response)
}

/** One row of `listStoredCredentials`. */
export type StoredCredential = {
  id: string
  /** The scheme whose credential this is. */
  credentialScheme: AuthSchemeName
  /** The scheme that verified the login. */
  loginScheme: AuthSchemeName
  issuedAt: Date
  expiresAt: Date
}

// PUBLIC API
/**
 * The user's live credentials that Wasp keeps in its own store: the rows a
 * "your devices" page shows and `signOutCredential` acts on. Rows issued
 * before the account's cut-off are left out, since they can never
 * authenticate again. Signed tokens have no row; handler-owned credentials
 * are the handler's to list. Throws when no scheme keeps credentials in the
 * database.
 */
export async function listStoredCredentials(user: UserRef): Promise<StoredCredential[]> {
  {=# isPrismaStoreUsed =}
  const auth = await findAuthWithUserBy({ userId: user.id })
  if (auth === null) {
    return []
  }
  const [account, rows] = await Promise.all([
    prisma.{= authEntityLower =}.findUnique({ where: { id: auth.id }, select: { credentialsInvalidatedAt: true } }),
    prisma.{= sessionEntityLower =}.findMany({
      where: { userId: auth.id, expiresAt: { gt: new Date() } },
      select: { id: true, credentialScheme: true, loginScheme: true, issuedAt: true, expiresAt: true },
      orderBy: { issuedAt: 'asc' },
    }),
  ])
  const cutOff = account?.credentialsInvalidatedAt ?? null
  return rows
    .filter((row) => cutOff === null || row.issuedAt >= cutOff)
    .map((row) => ({ ...row, credentialScheme: row.credentialScheme as AuthSchemeName, loginScheme: row.loginScheme as AuthSchemeName }))
  {=/ isPrismaStoreUsed =}
  {=^ isPrismaStoreUsed =}
  void user
  throw new Error('No auth scheme keeps its credentials in the database, so there is nothing to list.')
  {=/ isPrismaStoreUsed =}
}

// PUBLIC API
/**
 * Ends ONE credential by its id, from any request: what a "your active
 * sessions" page calls for the row the person clicked. Reaches every Wasp
 * credential kept in a store (`Session` rows, a custom store). A signed
 * token has no row and cannot be revoked before it expires, and a
 * handler-owned credential is the handler's to revoke, so this throws when
 * no scheme keeps credentials in a store at all. Unknown ids are a no-op.
 */
export async function signOutCredential(credentialId: string): Promise<void> {
  const acted = await signOutCredentialById(credentialId)
  if (!acted) {
    throw new Error(
      `Cannot sign out a credential by id: no auth scheme keeps its credentials in a store. A signed token cannot be revoked before it expires.`,
    )
  }
}

// PUBLIC API
/**
 * Ends EVERY credential of the user ("log out every device"), whoever issued
 * it. Lazy, like every framework that does this: a cut-off is stamped on the
 * account, and from then on Wasp refuses any credential issued before it,
 * its own and a handler's alike, as long as the handler reports
 * `credentialIssuedAt`. Nothing is deleted and no handler is called; dead
 * rows expire on their own.
 *
 * With `scheme`, ends only that scheme's credentials of the user, eagerly:
 * Wasp's own session rows of that scheme, or the handler's sessions through
 * its `signOutEverywhere`. Throws when the scheme cannot do that: a signed
 * token scheme (no rows; use the user-level form), a handler without
 * `signOutEverywhere`, or a scheme with no credentials of its own.
 */
export async function signOutEverywhere(user: UserRef, opts?: { scheme?: AuthSchemeName }): Promise<void> {
  const auth = await findAuthWithUserBy({ userId: user.id })
  if (auth === null) {
    throw new Error(`Cannot sign out everywhere: the user has no auth data.`)
  }
  if (opts?.scheme !== undefined) {
    await signOutEverywhereInScheme(opts.scheme, auth.id)
    return
  }
  await prisma.{= authEntityLower =}.update({ where: { id: auth.id }, data: { credentialsInvalidatedAt: new Date() } })
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
  await sendWebResponse(res, await challengeScheme(opts?.scheme ?? defaultScheme, toWebRequest(req)))
}

// PUBLIC API
/** Answers "you may not" the way `scheme` would: a 403 by default. */
export async function forbid(
  req: ExpressRequest,
  res: ExpressResponse,
  opts?: { scheme?: AuthSchemeName },
): Promise<void> {
  await sendWebResponse(res, await forbidScheme(opts?.scheme ?? defaultScheme, toWebRequest(req)))
}
