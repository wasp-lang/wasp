import { auth } from "./lucia.js";
import { throwInvalidCredentialsError } from "./utils.js";
import { prisma } from 'wasp/server';
import { createAuthUser } from "../server/auth/user.js";
// PRIVATE API
// Creates a new session for the `authId` in the database
export async function createSession(authId) {
    return auth.createSession(authId, {});
}
// PRIVATE API
export async function getSessionAndUserFromBearerToken(req) {
    const authorizationHeader = req.headers["authorization"];
    if (typeof authorizationHeader !== "string") {
        return {
            user: null,
            session: null,
        };
    }
    const sessionId = auth.readBearerToken(authorizationHeader);
    if (!sessionId) {
        return {
            user: null,
            session: null,
        };
    }
    return getSessionAndUserFromSessionId(sessionId);
}
// PRIVATE API
export async function getSessionAndUserFromSessionId(sessionId) {
    const { session, user: authEntity } = await auth.validateSession(sessionId);
    if (!session || !authEntity) {
        return {
            user: null,
            session: null,
        };
    }
    return {
        session,
        user: await getUser(authEntity.userId)
    };
}
async function getUser(userId) {
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
        throwInvalidCredentialsError();
    }
    return createAuthUser(user);
}
// PRIVATE API
export function invalidateSession(sessionId) {
    return auth.invalidateSession(sessionId);
}
//# sourceMappingURL=session.js.map