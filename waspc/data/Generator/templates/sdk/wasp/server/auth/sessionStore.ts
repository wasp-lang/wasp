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
   * The external provider's own session id when this session was minted by
   * credential exchange (`POST /auth/login`); lets logout revoke the provider's
   * session too (dual sign-out). Null for sessions issued by Wasp's own auth.
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
  options?: { providerSessionId?: string },
): Promise<{ id: string }> {
  const session = await lucia.createSession(authId, {
    providerSessionId: options?.providerSessionId ?? null,
  });
  return { id: session.id };
}

// PRIVATE API
// Validates a session token: returns the live session (bumping expiry when the
// implementation does) or null for a missing/expired one.
export async function validateSession(token: string): Promise<StoredSession | null> {
  const { session } = await lucia.validateSession(token);
  if (!session) {
    return null;
  }
  return {
    id: session.id,
    authId: session.userId,
    providerSessionId: session.providerSessionId,
  };
}

// PRIVATE API
// Reads a session row without validating it -- logout needs the stored
// `providerSessionId` even for a session it is about to delete.
export async function getStoredSession(sessionId: string): Promise<StoredSession | null> {
  const session = await prisma.{= sessionEntityLower =}.findUnique({
    where: { id: sessionId },
    select: { id: true, userId: true, providerSessionId: true },
  });
  if (!session) {
    return null;
  }
  return { id: session.id, authId: session.userId, providerSessionId: session.providerSessionId };
}

// PRIVATE API
export async function getStoredSessionsForAuthId(authId: string): Promise<StoredSession[]> {
  const sessions = await prisma.{= sessionEntityLower =}.findMany({
    where: { userId: authId },
    select: { id: true, userId: true, providerSessionId: true },
  });
  return sessions.map((s) => ({ id: s.id, authId: s.userId, providerSessionId: s.providerSessionId }));
}

// PRIVATE API
export function revokeSession(sessionId: string): Promise<void> {
  return lucia.invalidateSession(sessionId);
}

// PRIVATE API
export function revokeAllSessions(authId: string): Promise<void> {
  return lucia.invalidateUserSessions(authId);
}
