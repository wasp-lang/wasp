import { auth } from "./lucia.js";
import { throwInvalidCredentialsError, deserializeAndSanitizeProviderData, } from "./utils.js";
import { prisma } from 'wasp/server';
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
    // TODO: This logic must match the type in _types/index.ts (if we remove the
    // password field from the object here, we must to do the same there).
    // Ideally, these two things would live in the same place:
    // https://github.com/wasp-lang/wasp/issues/965
    const deserializedIdentities = user.auth.identities.map((identity) => {
        const deserializedProviderData = deserializeAndSanitizeProviderData(identity.providerData, {
            shouldRemovePasswordField: true,
        });
        return Object.assign(Object.assign({}, identity), { providerData: deserializedProviderData });
    });
    return Object.assign(Object.assign({}, user), { auth: Object.assign(Object.assign({}, user.auth), { identities: deserializedIdentities }) });
}
// PRIVATE API
export function invalidateSession(sessionId) {
    return auth.invalidateSession(sessionId);
}
//# sourceMappingURL=session.js.map