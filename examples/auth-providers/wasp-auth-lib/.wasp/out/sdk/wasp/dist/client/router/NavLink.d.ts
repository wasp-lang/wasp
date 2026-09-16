import { NavLink as RouterNavLink } from 'react-router';
import { type Routes } from './index';
type RouterNavLinkProps = Parameters<typeof RouterNavLink>[0];
export declare function NavLink({ to, params, search, hash, ...restOfProps }: Omit<RouterNavLinkProps, "to"> & {
    search?: Record<string, string>;
    hash?: string;
} & Routes): React.JSX.Element;
export {};
//# sourceMappingURL=NavLink.d.ts.map