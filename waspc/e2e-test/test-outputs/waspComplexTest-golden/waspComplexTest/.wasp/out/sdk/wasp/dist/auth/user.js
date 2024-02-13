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
// PUBLIC API
export function findUserIdentity(user, providerName) {
    return user.auth.identities.find((identity) => identity.providerName === providerName);
}
//# sourceMappingURL=user.js.map