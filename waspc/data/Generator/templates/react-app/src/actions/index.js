import {
  useMutation,
  useQueryClient,
} from 'react-query'

export { configureQueryClient } from '../queryClient'

export function useAction(actionFn, actionOptions) {
  const queryClient = useQueryClient();

  let options = {}

  if (actionOptions?.optimisticUpdates) {
    const optimisticUpdateConfigs = actionOptions.optimisticUpdates.map(translateToInternalConfig)
    options = makeRqOptimisticUpdateOptions(queryClient, optimisticUpdateConfigs)
  }

  return useMutation(actionFn, options).mutateAsync
}

function translateToInternalConfig(optimisticUpdateConfig) {
  const { getQuerySpecifier, ...rest } = optimisticUpdateConfig
  return {
    getQueryKey: (item) => getRqQueryKeyFromSpecifier(getQuerySpecifier(item)),
    ...rest,
  }
}

/**
 * This function implements the methods necessary for configuring optimistic
 * updates using React Query, as described by their documentation:
 * https://tanstack.com/query/v4/docs/guides/optimistic-updates?from=reactQueryV3&original=https://react-query-v3.tanstack.com/guides/optimistic-updates
 *
 * @param {QueryClient} queryClient The QueryClient instance used by React
 * Query.
 * @param {object} optimisticUpdateConfigs A list containing information on performing optimistic updates.
 * @returns An object containing 'onMutate' and 'onError' functions appropriate for the given config (check React Query's docs for details).
 */
function makeRqOptimisticUpdateOptions(queryClient, optimisticUpdateConfigs) {
  async function onMutate(item) {
    const specificOptimisticUpdateConfigs = optimisticUpdateConfigs.map(
      optimisticUpdateConfig => getOptimisticUpdateConfigForSpecificItem(optimisticUpdateConfig, item)
    )

    // Cancel any outgoing refetches (so they don't overwrite our optimistic update).
    // Theoretically, we can be a bit faster. Instead of awaiting the
    // cancellation of all queries, we could cancel and update them in parallel.
    // However, awaiting cancellation hasn't yet proven to be a performance bottleneck.
    await Promise.all(specificOptimisticUpdateConfigs.map(
      ({ query }) => queryClient.cancelQueries(query)
    ))

    // We're using a Map to to correctly serialize query keys that contain objects
    const previousData = new Map()
    specificOptimisticUpdateConfigs.forEach(({ queryKey, updateQuery }) => {
      // Snapshot the previous value
      const previousDataForQuery = queryClient.getQueryData(queryKey)

      // Optimistically update to the new value
      const updateFn = (old) => updateQuery(item, old)
      queryClient.setQueryData(queryKey, updateFn)

      // Remember the snapshotted value to restore in case of an error
      previousData.set(queryKey, previousDataForQuery)
    })

    return previousData
  }

  function onError(err, item, context) {
    context.previousData.forEach(async (data, queryKey) => {
      await queryClient.cancelQueries(queryKey)
      queryClient.setQueryData(queryKey, data)
    })
  }

  return {
    onMutate,
    onError,
  }
}

/**
 * Constructs the config needed to optimistically update a specific item. It
 * uses a closure over the updated to construct an item-specific query key
 * (e.g., when the query key depends on an ID)
 * 
 * @param {object} optimisticUpdateConfig  The general, "uninstantiated" optimistic
 * update config that contains a function for constructing a query key.
 * @param {*} item The item supposed to be optimisticallly updated.
 * @returns A specific, "instantiated" optimistic update config which contains a
 * fully-constructed query key
 */
function getOptimisticUpdateConfigForSpecificItem(optimisticUpdateConfig, item) {
  const { getQueryKey, ...remainingConfig } = optimisticUpdateConfig
  return {
    queryKey: getQueryKey(item),
    ...remainingConfig
  }
}

function getRqQueryKeyFromSpecifier(querySpecifier) {
  const [queryFn, ...otherKeys] = querySpecifier
  return [queryFn.queryCacheKey, ...otherKeys]
}
