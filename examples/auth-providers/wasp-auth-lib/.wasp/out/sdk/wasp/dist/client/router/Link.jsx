import { useMemo } from 'react';
import { Link as RouterLink } from 'react-router';
import { interpolatePath } from './linkHelpers';
// PUBLIC API
export function Link({ to, params, search, hash, ...restOfProps }) {
    const toPropWithParams = useMemo(() => {
        return interpolatePath(to, params, search, hash);
    }, [to, params, search, hash]);
    return <RouterLink to={toPropWithParams} {...restOfProps}/>;
}
//# sourceMappingURL=Link.jsx.map