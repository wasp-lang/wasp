import { Request as ExpressRequest } from "express";

import { type AuthUserData } from '../../auth/user.js';

import { authProvider } from "./provider/index.js";
import { canManageSessions, canRevokeSessions, type VerifiedSession } from "./provider/types.js";

import { config, prisma } from '../index.js';
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
 * every provider has -- see `SessionManagingAuthProvider`.
 */

// PRIVATE API
export type SessionAndUser = {
  sessionId: string;
  user: AuthUserData;
}

// PRIVATE API
// Creates a new session for the `authId` in the database.
export async function createSession(authId: string): Promise<{ id: string }> {
  const { sessionId } = await requireSessionManagingProvider().issueSession(authId);
  return { id: sessionId };
}

// PRIVATE API
export async function getSessionAndUserFromBearerToken(req: ExpressRequest): Promise<SessionAndUser | null> {
  const result = await authProvider.authenticate(toWebRequest(req));
  return result.status === 'authenticated' ? toSessionAndUser(result.session) : null;
}

// PRIVATE API
// Authenticates a bare credential with no surrounding request -- websockets hand
// us a token out of `socket.handshake.auth` rather than an HTTP request. The
// synthesized request carries only the `Authorization` header, which is why the
// provider contract requires authenticating from headers alone.
export async function getSessionAndUserFromSessionId(sessionId: string): Promise<SessionAndUser | null> {
  const request = new Request(config.serverUrl, {
    headers: { authorization: `Bearer ${sessionId}` },
  });
  const result = await authProvider.authenticate(request);
  return result.status === 'authenticated' ? toSessionAndUser(result.session) : null;
}

/**
 * Providers speak standard web `Request`, not Express -- an external provider's
 * SDK (Clerk's, Better Auth's) natively consumes one, and it keeps the contract
 * free of Express. This is the single place an Express request is converted.
 */
function toWebRequest(req: ExpressRequest): Request {
  const headers = new Headers();
  for (const [key, value] of Object.entries(req.headers)) {
    if (typeof value === 'string') {
      headers.set(key, value);
    } else if (Array.isArray(value)) {
      headers.set(key, value.join(', '));
    }
  }

  const host = req.get('host') ?? 'localhost';
  return new Request(`${req.protocol}://${host}${req.originalUrl}`, {
    method: req.method,
    headers,
  });
}

/**
 * Turns a verified session into the user data Wasp exposes as `context.user`.
 *
 * This is the step that guarantees `context.user` is always the developer's own
 * `User` entity, whichever provider vouched for the request.
 *
 * We look the user up *through* the auth entity rather than by its own id, which
 * keeps this to a single query and means the provider only ever has to tell us
 * which auth subject it verified.
 */
async function toSessionAndUser({ sessionId, subjectId }: VerifiedSession): Promise<SessionAndUser | null> {
  // Wasp's own auth owns the auth entity, so the subject id already identifies one.
  const authId = subjectId;

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

  return { sessionId, user: createAuthUserData(user) };
}

// PRIVATE API
// Ends the session server-side where the provider is able to. A pure token
// verifier has nothing to revoke -- there, the client dropping its credential
// is the whole logout, and this resolves without doing anything.
export function invalidateSession(sessionId: string): Promise<void> {
  return canRevokeSessions(authProvider)
    ? authProvider.revokeSession(sessionId)
    : Promise.resolve();
}

// PRIVATE API
// Invalidates all sessions belonging to the `authId` in the database
export function invalidateAllSessionsForAuthId(authId: string): Promise<void> {
  return requireSessionManagingProvider().revokeAllSessions(authId);
}

/**
 * Not every provider can mint or bulk-revoke sessions server-side -- a hosted one
 * may run those flows entirely in its own cloud. Wasp's own auth can, so this never
 * throws today; it exists so the constraint is explicit rather than assumed.
 */
function requireSessionManagingProvider() {
  if (!canManageSessions(authProvider)) {
    throw new Error(
      `The "${authProvider.id}" auth provider cannot manage sessions server-side.`
    );
  }
  return authProvider;
}
