import { useMemo } from 'react'
import { Link as RouterLink } from 'react-router-dom'
import { type Routes } from '../router'
import { interpolatePath } from './linkHelpers'

type RouterLinkProps = Parameters<typeof RouterLink>[0]

export function Link(
  { to, params, search, hash, ...restOfProps }: Omit<RouterLinkProps, "to">
  & {
    search?: Record<string, string>;
    hash?: string;
  }
  & Routes
) {
  const toPropWithParams = useMemo(() => {
    return interpolatePath(to, params, search, hash)
  }, [to, params])
  return <RouterLink to={toPropWithParams} {...restOfProps} />
}
