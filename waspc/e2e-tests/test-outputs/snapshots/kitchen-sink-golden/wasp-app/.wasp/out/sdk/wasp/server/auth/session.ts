import { Request as ExpressRequest } from "express";

import { type AuthUserData } from '../../auth/user.js';

import { canRevokeSessions } from "./provider/types.js";
import { getAuthProvider } from "./provider/index.js";
import * as sessionStore from "./sessionStore.js";

import { prisma } from '../index.js';
import { createAuthUserData } from "../../auth/user.js";

/**
 * Wasp's session layer.
 *
 * Every request is authenticated against a session Wasp itself minted, whichever
 * provider verified the login -- the classic full-stack-framework model (Rails,
 * Django, ASP.NET Core). A provider is consulted exactly twice: once at login,
 * when `POST /auth/login/:providerId` exchanges its credential for a Wasp
 * session, and once at logout, when the provider's own session is revoked
 * alongside Wasp's (dual sign-out, same as ASP.NET Core's two-scheme `SignOut`).
 *
 * Every session records the id of the provider that minted it, so logout and
 * user code always know which provider vouched for the login without ever
 * asking the providers.
 *
 * Known and accepted gap: revocation on the provider's side does NOT end the Wasp
 * session -- it lives until it expires or the user logs out. This is the same
 * trade-off ASP.NET Core's cookie makes after an OIDC login.
 */

// PRIVATE API
export type SessionAndUser = {
  sessionId: string;
  user: AuthUserData;
}

// PRIVATE API
// Creates a new session for the `authId` in the database. This is the mint
// path of Wasp's own auth flows (login forms, OAuth callbacks), so the session
// records 'wasp' as its minting provider; external providers mint through the
// credential exchange instead.
export async function createSession(authId: string): Promise<{ id: string }> {
  return sessionStore.createSession(authId, { providerId: 'wasp' });
}

// PRIVATE API
export async function getSessionAndUserFromBearerToken(req: ExpressRequest): Promise<SessionAndUser | null> {
  const token = sessionStore.getBearerToken(req.headers.authorization);
  return token === null ? null : getSessionAndUserFromSessionId(token);
}

// PRIVATE API
// Authenticates a bare session token with no surrounding request -- websockets
// hand us a token out of `socket.handshake.auth` rather than an HTTP request.
export async function getSessionAndUserFromSessionId(sessionId: string): Promise<SessionAndUser | null> {
  const session = await sessionStore.validateSession(sessionId);
  if (session === null) {
    return null;
  }
  return loadSessionAndUser(session.id, session.authId, session.providerId);
}

/**
 * Turns a validated session into the user data Wasp exposes as `context.user`.
 *
 * This is the step that guarantees `context.user` is always the developer's own
 * `User` entity, whichever provider vouched for the login.
 *
 * We look the user up *through* the auth entity rather than by its own id, which
 * keeps this to a single query.
 */
async function loadSessionAndUser(sessionId: string, authId: string, sessionProviderId: string): Promise<SessionAndUser | null> {
  const user = await prisma.user.findFirst({
    where: { auth: { id: authId } },
    include: {
      auth: {
        include: {
          identities: true
        }
      }
    }
  });

  // An auth entity that isn't linked to a user can't identify anyone, so we treat
  // the session as unauthenticated rather than erroring.
  if (!user) {
    return null;
  }

  return { sessionId, user: createAuthUserData(user, sessionProviderId) };
}


// PRIVATE API
/**
 * Dual sign-out, ASP.NET Core style: Wasp's session is always revoked, and when
 * its minting provider can revoke its own session, that one is revoked too.
 * The session row recorded which provider minted it, so the revocation always
 * goes to the right provider. The local revocation is what logs the user out;
 * the upstream one is best-effort -- its failure is logged, never surfaced, so
 * logout cannot be blocked by a provider outage.
 */
export async function invalidateSession(sessionId: string): Promise<void> {
  const stored = await sessionStore.getStoredSession(sessionId);
  await sessionStore.revokeSession(sessionId);

  if (stored?.providerSessionId != null && stored.providerId != null) {
    const provider = getAuthProvider(stored.providerId);
    if (provider !== undefined && canRevokeSessions(provider)) {
      try {
        await provider.revokeSession(stored.providerSessionId);
      } catch (error) {
        console.error(
          'Wasp session revoked, but revoking the auth provider session failed:',
          error,
        );
      }
    }
  }
}

// PRIVATE API
// Invalidates all of the auth entity's sessions, upstream ones included where
// each session's minting provider can revoke (same best-effort semantics as
// `invalidateSession`).
export async function invalidateAllSessionsForAuthId(authId: string): Promise<void> {
  const stored = await sessionStore.getStoredSessionsForAuthId(authId);
  await sessionStore.revokeAllSessions(authId);

  for (const session of stored) {
    if (session.providerSessionId == null || session.providerId == null) {
      continue;
    }
    const provider = getAuthProvider(session.providerId);
    if (provider === undefined || !canRevokeSessions(provider)) {
      continue;
    }
    try {
      await provider.revokeSession(session.providerSessionId);
    } catch (error) {
      console.error(
        'Wasp session revoked, but revoking the auth provider session failed:',
        error,
      );
    }
  }
}
