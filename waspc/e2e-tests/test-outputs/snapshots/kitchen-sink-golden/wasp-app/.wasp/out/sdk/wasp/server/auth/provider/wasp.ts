import { type Request as ExpressRequest } from 'express'

import { auth as lucia } from '../lucia.js'
import {
  type SessionIssuingAuthProvider,
  type VerifiedSession,
} from './types.js'

// PRIVATE API
/**
 * Wasp's own authentication, expressed as an `AuthProvider`.
 *
 * This is a thin adapter over the existing Lucia-backed session layer -- it changes
 * no behaviour. Its purpose is to be the first implementation of the provider
 * interface, so that Wasp's internals stop depending on Lucia directly and a second
 * provider can be added later without touching the middleware, the websocket
 * handler or the logout route.
 *
 * Wasp's auth owns the `Auth` and `Session` tables, so a subject id here *is* an
 * `Auth` entity id and needs no further resolution.
 */
export const waspAuthProvider: SessionIssuingAuthProvider = {
  id: 'wasp',

  async verifyRequest(req: ExpressRequest): Promise<VerifiedSession | null> {
    const authorizationHeader = req.headers['authorization']

    if (typeof authorizationHeader !== 'string') {
      return null
    }

    const sessionId = lucia.readBearerToken(authorizationHeader)
    if (!sessionId) {
      return null
    }

    return this.verifyCredential(sessionId)
  },

  async verifyCredential(credential: string): Promise<VerifiedSession | null> {
    const { session, user: authEntity } = await lucia.validateSession(credential)

    if (!session || !authEntity) {
      return null
    }

    // An `Auth` row that isn't linked to a user can't identify anyone, so we treat
    // the session as unauthenticated rather than erroring.
    if (authEntity.userId === null) {
      return null
    }

    return { sessionId: session.id, subjectId: session.userId }
  },

  async issueSession(subjectId: string): Promise<VerifiedSession> {
    const session = await lucia.createSession(subjectId, {})
    return { sessionId: session.id, subjectId }
  },

  revokeSession(sessionId: string): Promise<void> {
    return lucia.invalidateSession(sessionId)
  },

  revokeAllSessions(subjectId: string): Promise<void> {
    return lucia.invalidateUserSessions(subjectId)
  },
}
