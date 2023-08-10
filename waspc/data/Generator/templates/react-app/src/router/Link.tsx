import { useMemo } from 'react'
import { Link as RouterLink } from 'react-router-dom'
import { type Routes, interpolatePath } from './routes'

type RouterLinkProps = Parameters<typeof RouterLink>[0]

export function Link({ to, params, ...restOfProps }: RouterLinkProps & Routes) {
  const toWithParams = useMemo(() => {
    return interpolatePath(to, params)
  }, [to, params])
  return <RouterLink to={toWithParams} {...restOfProps} />
}
