import { useMemo } from 'react'
import { Link as RouterLink } from 'react-router-dom'
import { interpolatePath } from './linkHelpers'
import { type Routes } from './index'

type RouterLinkProps = Parameters<typeof RouterLink>[0]

// PUBLIC API
export function Link(
  { to, params, search, hash, ...restOfProps }: Omit<RouterLinkProps, "to">
  & {
    search?: Record<string, string>;
    hash?: string;
  }
  & Routes
): React.JSX.Element {
  const toPropWithParams = useMemo(() => {
    return interpolatePath(to, params, search, hash)
  }, [to, params])
  return <RouterLink to={toPropWithParams} {...restOfProps} />
}
