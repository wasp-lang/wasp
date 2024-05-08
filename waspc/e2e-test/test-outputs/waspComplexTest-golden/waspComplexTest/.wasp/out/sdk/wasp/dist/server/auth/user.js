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
import { deserializeAndSanitizeProviderData } from '../../auth/utils.js';
// PRIVATE API
export function createAuthUserData(user) {
    const { auth } = user, rest = __rest(user, ["auth"]);
    if (!auth) {
        throw new Error(`🐝 Error: trying to create a user without auth data.
This should never happen, but it did which means there is a bug in the code.`);
    }
    const identities = {
        google: getProviderInfo(auth, 'google'),
    };
    return Object.assign(Object.assign({}, rest), { identities });
}
function getProviderInfo(auth, providerName) {
    const identity = getIdentity(auth, providerName);
    if (!identity) {
        return null;
    }
    return Object.assign(Object.assign({}, deserializeAndSanitizeProviderData(identity.providerData, {
        shouldRemovePasswordField: true,
    })), { id: identity.providerUserId });
}
function getIdentity(auth, providerName) {
    var _a;
    return (_a = auth.identities.find((i) => i.providerName === providerName)) !== null && _a !== void 0 ? _a : null;
}
//# sourceMappingURL=user.js.map