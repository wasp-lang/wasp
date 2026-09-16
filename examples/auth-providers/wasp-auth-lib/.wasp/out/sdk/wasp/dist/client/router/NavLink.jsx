import { useMemo } from 'react';
import { NavLink as RouterNavLink } from 'react-router';
import { interpolatePath } from './linkHelpers';
// PUBLIC API
export function NavLink({ to, params, search, hash, ...restOfProps }) {
    const toPropWithParams = useMemo(() => {
        return interpolatePath(to, params, search, hash);
    }, [to, params, search, hash]);
    return <RouterNavLink to={toPropWithParams} {...restOfProps}/>;
}
//# sourceMappingURL=NavLink.jsx.map