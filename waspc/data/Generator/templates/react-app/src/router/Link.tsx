import { useMemo } from 'react'
import { Link as RouterLink } from 'react-router-dom'
import { type Routes, interpolatePath } from './routes'

type RouterLinkProps = Parameters<typeof RouterLink>[0]

export function Link(
  { to, params, search, hash, ...restOfProps }: Omit<RouterLinkProps, "to" | "params" | "search" | "hash">
  & {
    search?: Record<string, string>;
    hash?: string;
  }
  & Routes
) {
  const toWithParams = useMemo(() => {
    return interpolatePath(to, params, search, hash)
  }, [to, params])
  return <RouterLink to={toWithParams} {...restOfProps} />
}
