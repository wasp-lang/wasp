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
import { useMemo } from 'react';
import { Link as RouterLink } from 'react-router-dom';
import { interpolatePath } from './linkHelpers';
// PUBLIC API
export function Link(_a) {
    var { to, params, search, hash } = _a, restOfProps = __rest(_a, ["to", "params", "search", "hash"]);
    const toPropWithParams = useMemo(() => {
        return interpolatePath(to, params, search, hash);
    }, [to, params]);
    return <RouterLink to={toPropWithParams} {...restOfProps}/>;
}
//# sourceMappingURL=Link.jsx.map