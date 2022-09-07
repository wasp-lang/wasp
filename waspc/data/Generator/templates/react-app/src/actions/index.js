import {
  useMutation,
  useQueryClient,
} from 'react-query'

export { configureQueryClient } from '../queryClient'

/**
 * A hook for adding extra behavior to a Wasp action (e.g., optimistic updates).
 *
 * @param actionFn The Wasp action you wish to enhance.
 * @param {Object} actionOptions An options object for enhancing the given action.
 * @returns An enhanced function with an unchanged API.
 */
export function useAction(actionFn, actionOptions) {
  const queryClient = useQueryClient();

  let mutationFn = actionFn
  let options = {}

  if (actionOptions?.optimisticUpdates) {
    const optimisticUpdatesConfigs = actionOptions.optimisticUpdates.map(translateToInternalConfig)
    mutationFn = makeOptimisticUpdateMutationFn(actionFn, optimisticUpdatesConfigs)
    options = makeRqOptimisticUpdateOptions(queryClient, optimisticUpdatesConfigs)
  }

  // NOTE: We decided to hide React Query's extra mutation features (e.g.,
  // isLoading, onSuccess and onError callbacks, synchronous mutate) and only
  // expose a simple async function whose API matches the original action.
  const mutation = useMutation(mutationFn, options)
  return (args) => mutation.mutateAsync(args)
}

function translateToInternalConfig(optimisticUpdateConfig) {
  const { getQuerySpecifier, ...rest } = optimisticUpdateConfig
  return {
    getQueryKey: (item) => getRqQueryKeyFromSpecifier(getQuerySpecifier(item)),
    ...rest,
  }
}

function makeOptimisticUpdateMutationFn(actionFn, optimisticUpdatesConfig) {
  return function optimisticallyUpdateQueries(item) {
    const optimisticUpdateTuples = optimisticUpdatesConfig.map(
      ({ getQueryKey, updateQuery }) => ({
        queryKey: getQueryKey(item), 
        updateQueryFn: (old) => updateQuery(item, old),
      })
    ) 
    return actionFn.internal(item, optimisticUpdateTuples)
  }
}

/**
 * This function implements the methods necessary for configuring optimistic
 * updates using React Query, as described by their documentation:
 * https://tanstack.com/query/v4/docs/guides/optimistic-updates?from=reactQueryV3&original=https://react-query-v3.tanstack.com/guides/optimistic-updates
 *
 * @param {Object} queryClient The QueryClient instance used by React
 * Query.
 * @param {Object} optimisticUpdateConfigs A list containing information on performing optimistic updates.
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
      // Snapshot the currently cached value.
      const previousDataForQuery = queryClient.getQueryData(queryKey)

      // Attempt to optimistically update the cache using the new value.
      try {
        const updateFn = (old) => updateQuery(item, old)
        queryClient.setQueryData(queryKey, updateFn)
      } catch (e) {
        console.error("The `updateQuery` function threw an exception, skipping optimistic update:")
        console.error(e)
      }

      // Remember the snapshotted value to restore in case of an error.
      previousData.set(queryKey, previousDataForQuery)
    })

    return { previousData }
  }

  function onError(err, item, context) {
    // All we do in case of an error is roll back all optimistic updates. We ensure
    // not to do anything else because React Query rethrows the error. This allows
    // the programmer to handle the error as they usually would (i.e., we want the
    // error handling to work as it would if the programmer wasn't using optimistic
    // updates).
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
 * @param {Object} optimisticUpdateConfig  The general, "uninstantiated" optimistic
 * update config that contains a function for constructing a query key.
 * @param item The item supposed to be optimisticallly updated.
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

/**
 * Translates a Wasp query specifier to a query cache key used by React Query.
 * 
 * @returns A cache key React Query internally uses for addressing queries.
 */
function getRqQueryKeyFromSpecifier(querySpecifier) {
  const [queryFn, ...otherKeys] = querySpecifier
  return [queryFn.queryCacheKey, ...otherKeys]
}
