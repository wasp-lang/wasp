import {
  useMutation,
  useQueryClient,
} from 'react-query'
export { configureQueryClient } from '../queryClient'

export function useAction(actionFn, options) {
  const queryClient = useQueryClient();
  // todo: remove this
  window.queryClient = queryClient;
  const optimisticUpdate = options && options.optimisticUpdate ?
    getOptimisticUpdateFunctions(queryClient, options.optimisticUpdate) : {};

  return useMutation(actionFn, {
    ...optimisticUpdate,
  })
}

export function getOptimisticUpdateFunctions(queryClient, optimisticUpdate) {
  const {
    // how to make sure this is a query, a global query database?
    getQuery,
    updateFn
  } = optimisticUpdate

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
    onMutate,
    onError,
  }
}