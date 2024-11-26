import { isNotNull } from '../universal/predicates.js';
/**
 * We split the user.ts code into two files to avoid some server-only
 * code (Oslo's hashing functions) being imported on the client.
 */
// PUBLIC API
export function getEmail(user) {
    var _a, _b;
    return (_b = (_a = findUserIdentity(user, "email")) === null || _a === void 0 ? void 0 : _a.providerUserId) !== null && _b !== void 0 ? _b : null;
}
// PUBLIC API
export function getUsername(user) {
    var _a, _b;
    return (_b = (_a = findUserIdentity(user, "username")) === null || _a === void 0 ? void 0 : _a.providerUserId) !== null && _b !== void 0 ? _b : null;
}
// PUBLIC API
export function getFirstProviderUserId(user) {
    var _a;
    if (!user || !user.auth || !user.auth.identities || user.auth.identities.length === 0) {
        return null;
    }
    return (_a = user.auth.identities[0].providerUserId) !== null && _a !== void 0 ? _a : null;
}
export function makeAuthUserIfPossible(user) {
    return user ? makeAuthUser(user) : null;
}
function makeAuthUser(data) {
    return Object.assign(Object.assign({}, data), { getFirstProviderUserId: () => {
            const identities = Object.values(data.identities).filter(isNotNull);
            return identities.length > 0 ? identities[0].id : null;
        } });
}
function findUserIdentity(user, providerName) {
    var _a;
    if (!user.auth) {
        return null;
    }
    return (_a = user.auth.identities.find((identity) => identity.providerName === providerName)) !== null && _a !== void 0 ? _a : null;
}
//# sourceMappingURL=user.js.map