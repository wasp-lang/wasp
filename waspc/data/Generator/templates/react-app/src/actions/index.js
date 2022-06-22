import {
  useMutation,
  useQueryClient,
} from 'react-query'
export { configureQueryClient } from '../queryClient'

export function useAction(actionFn, options) {
  const queryClient = useQueryClient();
  // todo: remove this
  window.queryClient = queryClient;

  if () {
    return useMutation(actionFn)

  }
  const {
    mutationFn,
    options,
  } = !options || !options.optimisticUpdates ? { mutationFn: actionFn }
      : parseOptimisticUpdate(queryClient, actionFn, options.optimisticUpdates)


  return useMutation(mutationFn, options)
}

// todo: come up with a better name
function useOptimisticallyUpdatedMutation(queryClient, actionFn, optimisticUpdates) {
  // how to make sure this is a query, a global query database?
  function mutationFn(args) {
    optimisticUpdates.forEach(({ getQuery }) => {
      const key = getQuery(args)
      return actionFn.internal(args, [key])
    })
  }

  async function onMutate(item) {
    const previousData = {}

    await Promise.all(optimisticUpdates.map(async ({ getQuery, updateQuery }) => {
      const query = getQuery(item)
      await queryClient.cancelQueries(query)
      const previousDataForQuery = queryClient.getQueryData(query)
      queryClient.setQueryData(query, (old) => updateQuery(item, old))
      previousData[query] = previousDataForQuery
    }))

    return previousData
  }

  function onError(err, item, context) {
    optimisticUpdates.forEach(async ({ getQuery }) => {
      const query = getQuery(item)
      await queryClient.cancelQueries(query)
      queryClient.setQueryData(query, context.previousData[query])
    })
  }

  return {
    mutationFn,
    options: {
      onMutate,
      onError,
    }
  }
}