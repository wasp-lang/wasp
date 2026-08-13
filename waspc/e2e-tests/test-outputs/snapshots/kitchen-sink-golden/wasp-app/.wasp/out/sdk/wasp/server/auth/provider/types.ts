import { type Request as ExpressRequest } from 'express'
import { type FromRegister } from '../../../types/register.js'

// PRIVATE API
/**
 * The result of successfully verifying a credential.
 *
 * NOTE: the primitive here is *verify*, not *fetch*. A provider turns a credential
 * that arrived with the request into an identity. It is deliberately NOT
 * `findById(id)`: an external provider (Clerk, WorkOS) validates a signed token and
 * has no way to look a subject up by id on our behalf.
 */
export type VerifiedSession = {
  /**
   * Opaque, provider-owned id for this session.
   *
   * Wasp uses it for two things only: terminating the session on logout, and
   * authenticating websocket connections (which carry a bare token rather than a
   * `Request`). Wasp never interprets its contents.
   */
  sessionId: string
  /**
   * The provider's stable id for the authenticated subject.
   *
   * For in-process providers that own Wasp's auth tables (Wasp's own auth today)
   * this is the `Auth` entity's id. For external providers it will be the
   * provider's own user id, and Wasp resolves it to a local `Auth` row. That
   * second case does not exist yet -- see the provider-interface design doc.
   */
  subjectId: string
}

// PRIVATE API
/**
 * What Wasp's internals need from an authentication provider.
 *
 * This is the whole contract for the *session read path*: the request middleware,
 * websocket authentication, and logout. Everything users experience on top of it --
 * `authRequired` pages, `auth: true` operations, `context.user`, `useAuth()` --
 * is built by Wasp from these three methods and does not vary by provider.
 *
 * Establishing a session (login, signup) is deliberately NOT part of this
 * interface. Providers differ irreconcilably there: Clerk, for instance, has no
 * server-side password login at all. Those capabilities belong in separate,
 * optional interfaces so that Wasp can tell at compile time which features a given
 * provider can support.
 */
export interface AuthProvider {
  /**
   * Stable identifier for this provider, e.g. `'wasp'`.
   *
   * Used for diagnostics today. Once external providers exist it also becomes the
   * `providerName` under which their identities are recorded.
   */
  readonly id: string

  /**
   * Verify an incoming HTTP request.
   *
   * Returns `null` when the request carries no valid credential. That is *not* an
   * error -- Wasp lets unauthenticated requests through and leaves it to individual
   * operations to decide whether they require a user.
   */
  verifyRequest(req: ExpressRequest): Promise<VerifiedSession | null>

  /**
   * Verify a bare credential, with no surrounding request.
   *
   * Needed because websockets hand us a token out of `socket.handshake.auth`
   * rather than an HTTP request.
   */
  verifyCredential(credential: string): Promise<VerifiedSession | null>

  /** Terminate a single session. */
  revokeSession(sessionId: string): Promise<void>
}

// PRIVATE API
/**
 * A provider that can also mint sessions server-side.
 *
 * In-process providers (Wasp's own auth, Better Auth) can do this. Hosted ones may
 * not: Clerk's password verification lives on its Frontend API behind a
 * browser-held cookie, so there is no server-side call that turns credentials into
 * a session. Wasp's own login routes and auth UI require this capability.
 */
export interface SessionIssuingAuthProvider extends AuthProvider {
  /** Create a new session for a subject. */
  issueSession(subjectId: string): Promise<VerifiedSession>

  /** Terminate every session belonging to a subject. */
  revokeAllSessions(subjectId: string): Promise<void>
}

// PRIVATE API
/**
 * The provider the developer registered via `app.auth.provider`, if any.
 *
 * Declared here so that a user-written adapter is type-checked against the
 * contract at build time rather than failing somewhere inside the session layer.
 */
export type RegisteredAuthProvider = FromRegister<'authProvider', AuthProvider>

// PRIVATE API
/** Runtime capability check, mirroring the type-level distinction above. */
export function canIssueSessions(
  provider: AuthProvider,
): provider is SessionIssuingAuthProvider {
  return (
    typeof (provider as SessionIssuingAuthProvider).issueSession === 'function'
  )
}
