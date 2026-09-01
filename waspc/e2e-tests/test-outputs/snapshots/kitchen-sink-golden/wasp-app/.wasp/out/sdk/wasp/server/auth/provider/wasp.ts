import {
  type AuthenticateResult,
  type AuthProvider,
  type SupportsAllSessionsRevocation,
  type SupportsSessionRevocation,
  type VerifiedSession,
} from './types.js'

import * as sessionStore from '../sessionStore.js'

// PRIVATE API
/**
 * Wasp's own authentication, expressed as an `AuthProvider`.
 *
 * It is a thin adapter over Wasp's session store: since Wasp's auth owns the
 * `Auth` and `Session` tables, a subject id here *is* an `Auth` entity id and a
 * credential *is* a Wasp session token, so every method delegates to the store.
 * Its purpose is to keep Wasp's internals working against the same provider
 * interface an external adapter implements.
 *
 * Deliberately NOT a session issuer: wasp-auth's flows mint through the same
 * `wasp-sessions` runtime facet an adapter package requests (see
 * `waspAuthRuntime`), which is also where the app's login hooks fire -- a
 * separate mint path here would be a hook bypass waiting to happen.
 */
export const waspAuthProvider: AuthProvider &
  SupportsSessionRevocation &
  SupportsAllSessionsRevocation = {
  id: 'wasp',

  async authenticate(request: Request): Promise<AuthenticateResult> {
    const token = sessionStore.getBearerToken(request.headers.get('authorization'))
    if (token === null) {
      return { status: 'unauthenticated' }
    }

    const session = await sessionStore.validateSession(token)
    return session === null
      ? { status: 'unauthenticated' }
      : { status: 'authenticated', session: toVerifiedSession(session) }
  },

  revokeSession(sessionId: string): Promise<void> {
    return sessionStore.revokeSession(sessionId)
  },

  revokeAllSessions(subjectId: string): Promise<void> {
    return sessionStore.revokeAllSessions(subjectId)
  },
}

function toVerifiedSession(session: sessionStore.StoredSession): VerifiedSession {
  return { sessionId: session.id, subjectId: session.authId }
}
