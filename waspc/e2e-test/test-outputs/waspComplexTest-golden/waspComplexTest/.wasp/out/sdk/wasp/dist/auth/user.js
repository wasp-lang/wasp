var __rest = (this && this.__rest) || function (s, e) {
    var t = {};
    for (var p in s) if (Object.prototype.hasOwnProperty.call(s, p) && e.indexOf(p) < 0)
        t[p] = s[p];
    if (s != null && typeof Object.getOwnPropertySymbols === "function")
        for (var i = 0, p = Object.getOwnPropertySymbols(s); i < p.length; i++) {
            if (e.indexOf(p[i]) < 0 && Object.prototype.propertyIsEnumerable.call(s, p[i]))
                t[p[i]] = s[p[i]];
        }
    return t;
};
import { deserializeAndSanitizeProviderData } from './utils.js';
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
// PRIVATE API
export function createAuthUser(user) {
    const { auth } = user, rest = __rest(user, ["auth"]);
    const identities = {
        google: getProviderInfo(auth, 'google'),
    };
    return Object.assign(Object.assign({}, rest), { identities, getFirstProviderUserId: () => getFirstProviderUserId(user), 
        // Maybe useful for backwards compatibility? Full access?
        _rawUser: user });
}
function getProviderInfo(auth, providerName) {
    const identity = getIdentity(auth, providerName);
    if (!identity) {
        return null;
    }
    return {
        id: identity.providerUserId,
        data: deserializeAndSanitizeProviderData(identity.providerData, {
            shouldRemovePasswordField: true,
        }),
    };
}
function getIdentity(auth, providerName) {
    var _a;
    return (_a = auth.identities.find((i) => i.providerName === providerName)) !== null && _a !== void 0 ? _a : null;
}
//# sourceMappingURL=user.js.map