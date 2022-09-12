import {
  useMutation,
  useQueryClient,
} from 'react-query'

export { configureQueryClient } from '../queryClient'

/**
 * A hook for adding extra behavior to a Wasp Action (e.g., optimistic updates).
 *
 * @param actionFn The Wasp Action you wish to enhance/decorate.
 * @param {Object} actionOptions An options object for enhancing/decorating the given Action.
 * @returns {Function} A decorated Action with added behavior but an unchanged API.
 */
export function useAction(actionFn, actionOptions) {
  const queryClient = useQueryClient();

  let options = {}
  if (actionOptions?.optimisticUpdates) {
    const optimisticUpdateDefinitions = actionOptions.optimisticUpdates.map(translateToInternalDefinition)
    options = makeRqOptimisticUpdateOptions(queryClient, optimisticUpdateDefinitions)
  }

  // NOTE: We decided to hide React Query's extra mutation features (e.g.,
  // isLoading, onSuccess and onError callbacks, synchronous mutate) and only
  // expose a simple async function whose API matches the original Action.
  const mutation = useMutation(actionFn, options)
  return (args) => mutation.mutateAsync(args)
}

/**
 * Translates/Desugars a public optimistic update definition object into a definition object our
 * system uses internally.
 * 
 * @param {Object} publicOptimisticUpdateDefinition An optimistic update definition object that's a part of the public API:
 * https://wasp-lang.dev/docs/language/features#the-useaction-hook.
 * @returns {Object} An internally-used optimistic update definition object.
 */
function translateToInternalDefinition(publicOptimisticUpdateDefinition) {
  const { getQuerySpecifier, updateQuery } = publicOptimisticUpdateDefinition

  const definitionErrors = []
  if (typeof (getQuerySpecifier) !== 'function') {
    definitionErrors.push('`getQuerySpecifier` is not a function.')
  }
  if (typeof (updateQuery) !== 'function') {
    definitionErrors.push('`updateQuery` is not a function.')
  }
  if (definitionErrors.length) {
    throw new TypeError(`Invalid optimistic update definition: ${definitionErrors.join(', ')}.`)
  }

  return {
    getQueryKey: (item) => getRqQueryKeyFromSpecifier(getQuerySpecifier(item)),
    updateQuery,
  }
}

/**
 * This function implements the methods necessary for configuring optimistic
 * updates using React Query, as described by their documentation:
 * https://tanstack.com/query/v4/docs/guides/optimistic-updates?from=reactQueryV3&original=https://react-query-v3.tanstack.com/guides/optimistic-updates
 *
 * @param {Object} queryClient The QueryClient instance used by React Query.
 * @param {Object} optimisticUpdateDefinitions A list containing internal optimistic updates definition objects
 * (i.e., a list where each object carries the instructions for performing particular optimistic update).
 * @returns {Object} An object containing 'onMutate' and 'onError' functions corresponding to the given optimistic update
 * definitions (check React Query's docs for details).
 */
function makeRqOptimisticUpdateOptions(queryClient, optimisticUpdateDefinitions) {
  async function onMutate(item) {
    const optimisticUpdateDefinitionsForSpecificItem = optimisticUpdateDefinitions.map(
      optimisticUpdateConfig => getOptimisticUpdateConfigForSpecificItem(optimisticUpdateConfig, item)
    )

    // Cancel any outgoing refetches (so they don't overwrite our optimistic update).
    // Theoretically, we can be a bit faster. Instead of awaiting the
    // cancellation of all queries, we could cancel and update them in parallel.
    // However, awaiting cancellation hasn't yet proven to be a performance bottleneck.
    await Promise.all(optimisticUpdateDefinitionsForSpecificItem.map(
      ({ query }) => queryClient.cancelQueries(query)
    ))

    // We're using a Map to to correctly serialize query keys that contain objects
    const previousData = new Map()
    optimisticUpdateDefinitionsForSpecificItem.forEach(({ queryKey, updateQuery }) => {
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
 * Constructs the definition for optimistically updating a specific item. It
 * uses a closure over the updated item to construct an item-specific query key
 * (e.g., useful when the query key depends on an ID).
 *
 * @param {Object} optimisticUpdateDefinition The general, "uninstantiated" optimistic
 * update definition with a function for constructing the query key.
 * @param item The item triggering the Action/optimistic update (i.e., the argument passed to the Action).
 * @returns {Object} A specific, "instantiated" optimistic update config which contains a fully-constructed query key
 */
function getOptimisticUpdateConfigForSpecificItem(optimisticUpdateDefinition, item) {
  const { getQueryKey, ...remainingConfig } = optimisticUpdateDefinition
  return {
    queryKey: getQueryKey(item),
    ...remainingConfig
  }
}

/**
 * Translates a Wasp query specifier to a query cache key used by React Query.
 * 
 * @param {Object[]} querySpecifier A query specifier that's a part of the public API:
 * https://wasp-lang.dev/docs/language/features#the-useaction-hook
 * @returns {Object[]} A cache key React Query internally uses for addressing queries.
 */
function getRqQueryKeyFromSpecifier(querySpecifier) {
  const [queryFn, ...otherKeys] = querySpecifier
  return [queryFn.queryCacheKey, ...otherKeys]
}
