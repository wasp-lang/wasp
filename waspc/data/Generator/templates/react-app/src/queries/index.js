import { useQuery as rqUseQuery } from '@tanstack/react-query'
export { configureQueryClient } from '../queryClient'

export function useQuery(queryFn, queryFnArgs, options) {
  if (typeof queryFn !== 'function') {
    throw new TypeError('useQuery requires queryFn to be a function.')
  }
  if (!queryFn.queryCacheKey) {
    throw new TypeError('queryFn needs to have queryCacheKey property defined.')
  }

  const queryKey = queryFnArgs !== undefined ? [queryFn.queryCacheKey, queryFnArgs] : [queryFn.queryCacheKey]
  return rqUseQuery({
    queryKey,
    queryFn: () => queryFn(queryFnArgs),
    ...options
  })
}
