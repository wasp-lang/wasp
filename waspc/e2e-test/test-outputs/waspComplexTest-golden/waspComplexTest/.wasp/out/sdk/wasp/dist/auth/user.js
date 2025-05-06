import { isNotNull } from '../universal/predicates.js';
/**
 * We split the user.ts code into two files to avoid some server-only
 * code (Oslo's hashing functions) being imported on the client.
 */
// PUBLIC API
export function getEmail(user) {
    return findUserIdentity(user, "email")?.providerUserId ?? null;
}
// PUBLIC API
export function getUsername(user) {
    return findUserIdentity(user, "username")?.providerUserId ?? null;
}
// PUBLIC API
export function getFirstProviderUserId(user) {
    if (!user || !user.auth || !user.auth.identities || user.auth.identities.length === 0) {
        return null;
    }
    return user.auth.identities[0].providerUserId ?? null;
}
export function makeAuthUserIfPossible(user) {
    return user ? makeAuthUser(user) : null;
}
function makeAuthUser(data) {
    return {
        ...data,
        getFirstProviderUserId: () => {
            const identities = Object.values(data.identities).filter(isNotNull);
            return identities.length > 0 ? identities[0].id : null;
        },
    };
}
function findUserIdentity(user, providerName) {
    if (!user.auth) {
        return null;
    }
    return user.auth.identities.find((identity) => identity.providerName === providerName) ?? null;
}
//# sourceMappingURL=user.js.map