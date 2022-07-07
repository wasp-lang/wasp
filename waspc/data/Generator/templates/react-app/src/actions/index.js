import {
  useMutation,
  useQueryClient,
} from 'react-query'

export { configureQueryClient } from '../queryClient'

export function useAction(actionFn, actionOptions) {
  const queryClient = useQueryClient();

  let mutationFn = actionFn
  // TODO(filip): Do we allow all react query options or should we consider a list of allowed options?
  let options = {}

  if (actionOptions?.optimisticUpdates) {
    const optimisticUpdatesConfig = makeOptimisticUpdatesConfig(actionOptions.optimisticUpdates)
    mutationFn = makeOptimisticUpdateMutationFn(actionFn, optimisticUpdatesConfig)
    options = makeOptimisticUpdateOptions(queryClient, optimisticUpdatesConfig)
  }

  return useMutation(mutationFn, options)
}


function makeOptimisticUpdatesConfig(optimisticUpdatesConfig) {
  return optimisticUpdatesConfig.map(({ getQuery, ...rest }) => ({
    getQuery: (item) => parseQueryKey(getQuery(item)),
    ...rest,
  }))
}

function makeOptimisticUpdateMutationFn(actionFn, optimisticUpdatesConfig) {
  return function optimisticallyUpdateQueries(args) {
    optimisticUpdatesConfig.forEach(({ getQuery }) => {
      // how to make sure this is a query, a global query database?
      const key = getQuery(args)
      return actionFn.internal(args, [key])
    })
  }
}

function makeOptimisticUpdateOptions(queryClient, optimisticUpdatesConfig) {
  async function onMutate(item) {
    const queriesToUpdate = optimisticUpdatesConfig.map(({ getQuery, ...rest }) => ({
      query: getQuery(item),
      ...rest,
    }))

    const queryCancellations = queriesToUpdate.map(
      ({ query }) => queryClient.cancelQueries(query)
    )

    // Theoretically, we can be a bit faster. Instead of awaiting the
    // cancellation of all queries, we could cancel and update them in parallel.
    // However, awaiting cancellation probably doesn't take too much time.
    await Promise.all(queryCancellations)

    // We're using a Map to to correctly serialize query keys that contain objects
    const previousData = new Map()
    queriesToUpdate.forEach(({ query, updateQuery }) => {
      const previousDataForQuery = queryClient.getQueryData(query)
      queryClient.setQueryData(query, (old) => updateQuery(item, old))
      previousData.set(query, previousDataForQuery)
    })

    return previousData
  }

  function onError(err, item, context) {
    context.previousData.forEach(async (data, queryKey) => {
        await queryClient.cancelQueries(queryKey)
        queryClient.setQueryData(queryKey, data)
      }
    )
  }

  return {
    onMutate,
    onError,
  }
}

function parseQueryKey(queryKey) {
  const [queryFnOrCacheKey, ...otherKeys] = queryKey
  const queryCacheKey = typeof queryFnOrCacheKey == 'function' ? queryFnOrCacheKey.queryCacheKey : queryFnOrCacheKey;
  return [queryCacheKey, ...otherKeys]
}
