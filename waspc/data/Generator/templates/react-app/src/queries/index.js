import { useQuery as rqUseQuery } from 'react-query'

export const useQuery = (queryFn, queryFnArgs, config) => {
  if (typeof queryFn !== 'function') {
    throw new Error('useQuery requires queryFn to be a function.')
  }
  if (!queryFn.queryCacheKey) {
    throw new Error('queryFn needs to have queryCacheKey property defined.')
  }

  const rqResult = rqUseQuery({
    queryKey: [queryFn.queryCacheKey, queryFnArgs],
    queryFn: (_key, args) => queryFn(args),
    config
  })

  return rqResult
}
