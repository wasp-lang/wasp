{{={= =}=}}
import { Request as ExpressRequest } from "express";

import { type AuthUserData } from '../../auth/user.js';

import { authProvider, canIssueSessions } from "./provider/index.js";

import { prisma } from '../index.js';
import { createAuthUserData } from "../../auth/user.js";

/**
 * Wasp's session layer.
 *
 * Everything here is expressed in terms of the `AuthProvider` interface rather than
 * a concrete session library, so that swapping the provider does not reach into the
 * request middleware, the websocket handler or the logout route.
 *
 * The split is deliberate: *reading* an identity is uniform across providers and
 * lives here, while *establishing* one (login, signup) needs capabilities that not
 * every provider has -- see `SessionIssuingAuthProvider`.
 */

// PRIVATE API
export type SessionAndUser = {
  sessionId: string;
  user: AuthUserData;
}

// PRIVATE API
// Creates a new session for the `authId` in the database.
export async function createSession(authId: string): Promise<{ id: string }> {
  const { sessionId } = await requireSessionIssuingProvider().issueSession(authId);
  return { id: sessionId };
}

// PRIVATE API
export async function getSessionAndUserFromBearerToken(req: ExpressRequest): Promise<SessionAndUser | null> {
  const verified = await authProvider.verifyRequest(req);
  return verified === null ? null : toSessionAndUser(verified.sessionId, verified.subjectId);
}

// PRIVATE API
export async function getSessionAndUserFromSessionId(sessionId: string): Promise<SessionAndUser | null> {
  const verified = await authProvider.verifyCredential(sessionId);
  return verified === null ? null : toSessionAndUser(verified.sessionId, verified.subjectId);
}

/**
 * Turns a verified session into the user data Wasp exposes as `context.user`.
 *
 * This is the step that guarantees `context.user` is always the developer's own
 * `{= userEntityUpper =}` entity, whichever provider vouched for the request.
 *
 * We look the user up *through* the auth entity rather than by its own id, which
 * keeps this to a single query and means the provider only ever has to tell us
 * which auth subject it verified.
 */
async function toSessionAndUser(sessionId: string, authId: string): Promise<SessionAndUser | null> {
  const user = await prisma.{= userEntityLower =}.findFirst({
    where: { {= authFieldOnUserEntityName =}: { id: authId } },
    include: {
      {= authFieldOnUserEntityName =}: {
        include: {
          {= identitiesFieldOnAuthEntityName =}: true
        }
      }
    }
  });

  // An auth entity that isn't linked to a user can't identify anyone, so we treat
  // the session as unauthenticated rather than erroring.
  if (!user) {
    return null;
  }

  return { sessionId, user: createAuthUserData(user) };
}

// PRIVATE API
export function invalidateSession(sessionId: string): Promise<void> {
  return authProvider.revokeSession(sessionId);
}

// PRIVATE API
// Invalidates all sessions belonging to the `authId` in the database
export function invalidateAllSessionsForAuthId(authId: string): Promise<void> {
  return requireSessionIssuingProvider().revokeAllSessions(authId);
}

/**
 * Not every provider can mint or bulk-revoke sessions server-side -- a hosted one
 * may run those flows entirely in its own cloud. Wasp's own auth can, so this never
 * throws today; it exists so the constraint is explicit rather than assumed.
 */
function requireSessionIssuingProvider() {
  if (!canIssueSessions(authProvider)) {
    throw new Error(
      `The "${authProvider.id}" auth provider cannot manage sessions server-side.`
    );
  }
  return authProvider;
}
