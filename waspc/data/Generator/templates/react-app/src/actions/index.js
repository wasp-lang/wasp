import {
  useMutation,
  useQueryClient,
} from 'react-query'
export { configureQueryClient } from '../queryClient'

export function useAction(actionFn, options) {
  const queryClient = useQueryClient();
  // todo: remove this
  window.queryClient = queryClient;

  const {
    mutationFn,
    optimisticUpdate
  } = options && options.optimisticUpdate ?
      parseOptimisticUpdate(queryClient, actionFn, options.optimisticUpdate) : {
        mutationFn: actionFn,
        optimisticUpdate: {}
      };

  return useMutation(mutationFn, {
    ...optimisticUpdate,
  })
}

// todo: come up with a better name
function parseOptimisticUpdate(queryClient, actionFn, optimisticUpdate) {
  const {
    // how to make sure this is a query, a global query database?
    getQuery,
    updateFn
  } = optimisticUpdate

  function mutationFn(args) {
    const key = getQuery(args)
    return actionFn.internal(args, [key])
  }

  async function onMutate(item) {
    const query = getQuery(item)
    await queryClient.cancelQueries(query)
    const previousData = queryClient.getQueryData(query)
    queryClient.setQueryData(query, (old) => updateFn(item, old))
    return { previousData }
  }

  async function onError(err, item, context) {
    const query = getQuery(item)
    await queryClient.cancelQueries(query)
    queryClient.setQueryData(query, context.previousData)
  }

  return {
    mutationFn,
    optimisticUpdate: {
      onMutate,
      onError,
    }
  }
}