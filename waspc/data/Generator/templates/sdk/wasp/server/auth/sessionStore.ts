{{={= =}=}}
import { auth as lucia } from './lucia.js'

import { prisma } from '../index.js'

/**
 * Wasp's own session store: every session the app runs on -- whichever auth
 * provider verified the login -- is a row in the app's `{= sessionEntityUpper =}`
 * table, created and revoked through this module.
 *
 * Lucia is the implementation detail behind it. Nothing outside this module may
 * touch Lucia for session work, so migrating off it is a one-module change.
 */

// PRIVATE API
export type StoredSession = {
  id: string;
  authId: string;
  /**
   * Id of the provider that minted this session ('wasp', 'external:clerk',
   * ...). Every session records it at mint time, so logout revocation and
   * `user.sessionProviderId` always know which provider vouched for the
   * login. Null only on rows from before the column existed; `validateSession`
   * treats those as invalid.
   */
  providerId: string | null;
  /**
   * The external provider's own session id when this session was minted by
   * credential exchange (`POST /auth/login/:providerId`); lets logout revoke
   * the provider's session too (dual sign-out). Null for sessions issued by
   * Wasp's own auth.
   */
  providerSessionId: string | null;
}

// PRIVATE API
// The single place a session credential is parsed out of an `Authorization`
// header value, shared by the request middleware and Wasp's own provider.
export function getBearerToken(header: string | null | undefined): string | null {
  const prefix = 'Bearer ';
  if (typeof header !== 'string' || !header.startsWith(prefix)) {
    return null;
  }
  return header.substring(prefix.length);
}

// PRIVATE API
export async function createSession(
  authId: string,
  options: { providerId: string; providerSessionId?: string },
): Promise<{ id: string }> {
  const session = await lucia.createSession(authId, {
    providerId: options.providerId,
    providerSessionId: options.providerSessionId ?? null,
  });
  return { id: session.id };
}

// PRIVATE API
// Validates a session token: returns the live session (bumping expiry when the
// implementation does) or null for a missing/expired one. A session with no
// recorded minting provider (a row from before the providerId column existed)
// is treated as invalid and deleted -- one forced re-login buys the guarantee
// that logout revocation is never misattributed.
export async function validateSession(token: string): Promise<(StoredSession & { providerId: string }) | null> {
  const { session } = await lucia.validateSession(token);
  if (!session) {
    return null;
  }
  if (session.providerId === null) {
    await lucia.invalidateSession(session.id);
    return null;
  }
  return {
    id: session.id,
    authId: session.userId,
    providerId: session.providerId,
    providerSessionId: session.providerSessionId,
  };
}

// PRIVATE API
// Reads a session row without validating it -- logout needs the stored
// `providerSessionId` even for a session it is about to delete.
export async function getStoredSession(sessionId: string): Promise<StoredSession | null> {
  const session = await prisma.{= sessionEntityLower =}.findUnique({
    where: { id: sessionId },
    select: { id: true, userId: true, providerId: true, providerSessionId: true },
  });
  if (!session) {
    return null;
  }
  return { id: session.id, authId: session.userId, providerId: session.providerId, providerSessionId: session.providerSessionId };
}

// PRIVATE API
export async function getStoredSessionsForAuthId(authId: string): Promise<StoredSession[]> {
  const sessions = await prisma.{= sessionEntityLower =}.findMany({
    where: { userId: authId },
    select: { id: true, userId: true, providerId: true, providerSessionId: true },
  });
  return sessions.map((s) => ({ id: s.id, authId: s.userId, providerId: s.providerId, providerSessionId: s.providerSessionId }));
}

// PRIVATE API
export function revokeSession(sessionId: string): Promise<void> {
  return lucia.invalidateSession(sessionId);
}

// PRIVATE API
export function revokeAllSessions(authId: string): Promise<void> {
  return lucia.invalidateUserSessions(authId);
}
