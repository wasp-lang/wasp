import { auth } from "./lucia.js";
import { createInvalidCredentialsError } from "./utils.js";
import { prisma } from 'wasp/server';
import { createAuthUserData } from "../server/auth/user.js";
// PRIVATE API
// Creates a new session for the `authId` in the database
export async function createSession(authId) {
    return auth.createSession(authId, {});
}
// PRIVATE API
export async function getSessionAndUserFromBearerToken(req) {
    const authorizationHeader = req.headers["authorization"];
    if (typeof authorizationHeader !== "string") {
        return null;
    }
    const sessionId = auth.readBearerToken(authorizationHeader);
    if (!sessionId) {
        return null;
    }
    return getSessionAndUserFromSessionId(sessionId);
}
// PRIVATE API
export async function getSessionAndUserFromSessionId(sessionId) {
    const { session, user: authEntity } = await auth.validateSession(sessionId);
    if (!session || !authEntity) {
        return null;
    }
    return {
        session,
        user: await getAuthUserData(authEntity.userId)
    };
}
async function getAuthUserData(userId) {
    const user = await prisma.user
        .findUnique({
        where: { id: userId },
        include: {
            auth: {
                include: {
                    identities: true
                }
            }
        }
    });
    if (!user) {
        throw createInvalidCredentialsError();
    }
    return createAuthUserData(user);
}
// PRIVATE API
export function invalidateSession(sessionId) {
    return auth.invalidateSession(sessionId);
}
//# sourceMappingURL=session.js.map