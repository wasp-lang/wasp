import {
  useMutation,
  useQueryClient,
} from 'react-query'
export { configureQueryClient } from '../queryClient'

export function useAction(actionFn, options) {
  const queryClient = useQueryClient();
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

  return {
    async onMutate(item) {
      const query = getQuery(item)
      await queryClient.cancelQueries(query)
      const previousData = queryClient.getQueryData(query)
      queryClient.setQueryData(query, (old) => updateFn(item, old))
      return { previousData }
    },
    async onError(err, item, context) {
      const query = getQuery(item)
      await queryClient.cancelQueries(query)
      queryClient.setQueryData(query, context.previousData)
    },
    async onSettled(item) {
      const query = getQuery(item)
      queryClient.invalidateQueries(query)
    }
  }
}