import { parseProviderData, } from './providerData.js';
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
        // The identities map only carries Wasp's own auth methods, and none are
        // enabled without waspAuth among the providers, so there is nothing to
        // read. External identities are reachable server-side through the
        // identity store.
        getFirstProviderUserId: () => null,
    };
}
// PRIVATE API
export function createAuthUserData(user, sessionProviderId) {
    const { auth, ...rest } = user;
    if (!auth) {
        throw new Error(`🐝 Error: trying to create a user without auth data.
This should never happen, but it did which means there is a bug in the code.`);
    }
    const identities = {};
    return {
        ...rest,
        sessionProviderId: sessionProviderId,
        identities,
    };
}
function getProviderInfo(auth, providerName) {
    const identity = getIdentity(auth, providerName);
    if (!identity) {
        return null;
    }
    return {
        ...parseProviderData(identity.providerData),
        id: identity.providerUserId,
    };
}
function getIdentity(auth, providerName) {
    return auth.identities.find((i) => i.providerName === providerName) ?? null;
}
function findUserIdentity(user, providerName) {
    if (!user.auth) {
        return null;
    }
    return user.auth.identities.find((identity) => identity.providerName === providerName) ?? null;
}
//# sourceMappingURL=user.js.map