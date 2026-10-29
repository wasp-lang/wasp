import { useMemo } from 'react'
import { Link as RouterLink } from 'react-router'
import { interpolatePath, type OptionalRouteOptions } from '@wasp/lib-sdk-core'
import type { Routes } from './index'

type RouterLinkProps = Parameters<typeof RouterLink>[0]

// PUBLIC API
export type LinkProps = Omit<RouterLinkProps, "to">
  & OptionalRouteOptions
  & Routes

// PUBLIC API
export function Link(
  { to, params, search, hash, ...restOfProps }: LinkProps
): React.JSX.Element {
  const toPropWithParams = useMemo(() => {
    return interpolatePath(to, params, search, hash)
  }, [to, params, search, hash])
  return <RouterLink to={toPropWithParams} {...restOfProps} />
}
