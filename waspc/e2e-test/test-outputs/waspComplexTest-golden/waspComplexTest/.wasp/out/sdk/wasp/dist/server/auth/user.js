import { getProviderData, } from '../../auth/utils.js';
// PRIVATE API
export function createAuthUserData(user) {
    const { auth, ...rest } = user;
    if (!auth) {
        throw new Error(`🐝 Error: trying to create a user without auth data.
This should never happen, but it did which means there is a bug in the code.`);
    }
    const identities = {
        google: getProviderInfo(auth, 'google'),
    };
    return {
        ...rest,
        identities,
    };
}
function getProviderInfo(auth, providerName) {
    const identity = getIdentity(auth, providerName);
    if (!identity) {
        return null;
    }
    return {
        ...getProviderData(identity.providerData),
        id: identity.providerUserId,
    };
}
function getIdentity(auth, providerName) {
    return auth.identities.find((i) => i.providerName === providerName) ?? null;
}
//# sourceMappingURL=user.js.map