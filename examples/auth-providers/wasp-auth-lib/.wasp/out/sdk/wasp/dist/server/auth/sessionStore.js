import { auth as lucia } from './lucia.js';
import { prisma } from '../index.js';
// PRIVATE API
// The single place a session credential is parsed out of an `Authorization`
// header value, shared by the request middleware and Wasp's own provider.
export function getBearerToken(header) {
    const prefix = 'Bearer ';
    if (typeof header !== 'string' || !header.startsWith(prefix)) {
        return null;
    }
    return header.substring(prefix.length);
}
// PRIVATE API
export async function createSession(authId, options) {
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
export async function validateSession(token) {
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
export async function getStoredSession(sessionId) {
    const session = await prisma.session.findUnique({
        where: { id: sessionId },
        select: { id: true, userId: true, providerId: true, providerSessionId: true },
    });
    if (!session) {
        return null;
    }
    return { id: session.id, authId: session.userId, providerId: session.providerId, providerSessionId: session.providerSessionId };
}
// PRIVATE API
export async function getStoredSessionsForAuthId(authId) {
    const sessions = await prisma.session.findMany({
        where: { userId: authId },
        select: { id: true, userId: true, providerId: true, providerSessionId: true },
    });
    return sessions.map((s) => ({ id: s.id, authId: s.userId, providerId: s.providerId, providerSessionId: s.providerSessionId }));
}
// PRIVATE API
export function revokeSession(sessionId) {
    return lucia.invalidateSession(sessionId);
}
// PRIVATE API
export function revokeAllSessions(authId) {
    return lucia.invalidateUserSessions(authId);
}
//# sourceMappingURL=sessionStore.js.map