import { useMemo } from 'react'
import { NavLink as RouterNavLink } from 'react-router'
import { interpolatePath } from './linkHelpers'
import { type Routes } from './index'

type RouterNavLinkProps = Parameters<typeof RouterNavLink>[0]

// PUBLIC API
export function NavLink(
  { to, params, search, hash, ...restOfProps }: Omit<RouterNavLinkProps, "to">
  & {
    search?: Record<string, string>;
    hash?: string;
  }
  & Routes
): React.JSX.Element {
  const toPropWithParams = useMemo(() => {
    return interpolatePath(to, params, search, hash)
  }, [to, params, search, hash])
  return <RouterNavLink to={toPropWithParams} {...restOfProps} />
}
