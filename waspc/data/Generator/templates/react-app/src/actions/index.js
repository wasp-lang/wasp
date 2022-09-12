import {
  useMutation,
  useQueryClient,
} from 'react-query'

export { configureQueryClient } from '../queryClient'

/**
 * An options object passed into the `useAction` hook and used to enhance the
 * action with extra options.
 *
 * @typedef {Object} ActionOptions
 * @property {PublicOptimisticUpdateDefinition[]} optimisticUpdates
 */

/**
 * A documented (public) way to define optimistic updates.
 * 
 * @typedef {Object} PublicOptimisticUpdateDefinition
 * @property {GetQuerySpecifier} querySpecifier
 * @property {UpdateQuery} updateQuery
 */

/**
 * A function that takes an item and returns a Wasp Query specifier.
 * 
 * @callback GetQuerySpecifier
 * @param {T} item
 * @returns {QuerySpecifier}
 */

/**
 * A function that takes an item and the previous state of the cache, and returns
 * the desired (new) state of the cache.
 * 
 * @callback UpdateQuery
 * @param {T} item
 * @param {T[]} oldData
 * @returns {T[]}
 */

/**
 * A public query specifier used for addressing Wasp queries. See our docs for details:
 * https://wasp-lang.dev/docs/language/features#the-useaction-hook.
 * 
 * @typedef {any[]} QuerySpecifier
 */

/**
 * An internal (undocumented, private, desugared) way of defining optimistic updates.
 * 
 * @typedef {Object} InternalOptimisticUpdateDefinition
 * @property {GetQuerySpecifier} querySpecifier
 * @property {UpdateQuery} updateQuery
 */

/**
 * An UpdateQuery function "instantiated" with a specific item. It only takes
 * the current state of the cache and returns the desired (new) state of the
 * cache.
 * 
 * @callback SpecificUpdateQuery
 * @param {any[]} oldData
 */

/**
 * A specific, "instantiated" optimistic update definition which contains a
 * fully-constructed query key and a specific update function.
 * 
 * @typedef {Object} SpecificOptimisticUpdateDefinition
 * @property {QueryKey} queryKey
 * @property {SpecificUpdateQuery} updateQuery
*/

/**
 * An array React Query uses to address queries. See their docs for details:
 * https://react-query-v3.tanstack.com/guides/query-keys#array-keys.
 * 
 * @typedef {any[]} QueryKey
 */


/**
 * A hook for adding extra behavior to a Wasp Action (e.g., optimistic updates).
 *
 * @param actionFn The Wasp Action you wish to enhance/decorate.
 * @param {ActionOptions} actionOptions An options object for enhancing/decorating the given Action.
 * @returns A decorated Action with added behavior but an unchanged API.
 */
export function useAction(actionFn, actionOptions) {
  const queryClient = useQueryClient();

  let mutationFn = actionFn
  let options = {}
  if (actionOptions?.optimisticUpdates) {
    const optimisticUpdatesDefinitions = actionOptions.optimisticUpdates.map(translateToInternalDefinition)
    mutationFn = makeOptimisticUpdateMutationFn(actionFn, optimisticUpdatesDefinitions)
    options = makeRqOptimisticUpdateOptions(queryClient, optimisticUpdatesDefinitions)
  }

  // NOTE: We decided to hide React Query's extra mutation features (e.g.,
  // isLoading, onSuccess and onError callbacks, synchronous mutate) and only
  // expose a simple async function whose API matches the original Action.
  const mutation = useMutation(mutationFn, options)
  return (args) => mutation.mutateAsync(args)
}

/**
 * Translates/Desugars a public optimistic update definition object into a definition object our
 * system uses internally.
 * 
 * @param {PublicOptimisticUpdateDefinition} publicOptimisticUpdateDefinition An optimistic update definition
 * object that's a part of the public API: https://wasp-lang.dev/docs/language/features#the-useaction-hook.
 * @returns {InternalOptimisticUpdateDefinition} An internally-used optimistic update definition object.
 */
function translateToInternalDefinition(publicOptimisticUpdateDefinition) {
  const { getQuerySpecifier, updateQuery } = publicOptimisticUpdateDefinition

  const definitionErrors = []
  if (typeof getQuerySpecifier !== 'function') {
    definitionErrors.push('`getQuerySpecifier` is not a function.')
  }
  if (typeof updateQuery !== 'function') {
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
 * Creates a function that performs an action while telling it about the
 * optimistic updates it caused.
 * 
 * @param actionFn - The Wasp Action.
 * @param {InternalOptimisticUpdateDefinition} optimisticUpdateDefinitions - The optimisitc updates the 
 * action causes.
 * @returns A 
 */
function makeOptimisticUpdateMutationFn(actionFn, optimisticUpdateDefinitions) {
  return function performActionWithOptimisticUpdates(item) {
    const specificOptimisticUpdateDefinitions = optimisticUpdateDefinitions.map(
      generalDefinition => getOptimisticUpdateDefinitionForSpecificItem(generalDefinition, item)
    )
    return actionFn.internal(item, specificOptimisticUpdateDefinitions)
  }
}

/**
 * This function implements the methods necessary for configuring optimistic
 * updates using React Query, as described by their documentation:
 * https://tanstack.com/query/v4/docs/guides/optimistic-updates?from=reactQueryV3&original=https://react-query-v3.tanstack.com/guides/optimistic-updates
 *
 * @param {Object} queryClient The QueryClient instance used by React Query.
 * @param {InternalOptimisticUpdateDefinition} optimisticUpdateDefinitions A list containing internal optimistic updates definition objects
 * (i.e., a list where each object carries the instructions for performing particular optimistic update).
 * @returns {Object} An object containing 'onMutate' and 'onError' functions corresponding to the given optimistic update
 * definitions (check React Query's docs for details).
 */
function makeRqOptimisticUpdateOptions(queryClient, optimisticUpdateDefinitions) {
  async function onMutate(item) {
    const specificOptimisticUpdateDefinitions = optimisticUpdateDefinitions.map(
      generalDefinition => getOptimisticUpdateDefinitionForSpecificItem(generalDefinition, item)
    )

    // Cancel any outgoing refetches (so they don't overwrite our optimistic update).
    // Theoretically, we can be a bit faster. Instead of awaiting the
    // cancellation of all queries, we could cancel and update them in parallel.
    // However, awaiting cancellation hasn't yet proven to be a performance bottleneck.
    await Promise.all(specificOptimisticUpdateDefinitions.map(
      ({ queryKey }) => queryClient.cancelQueries(queryKey)
    ))

    // We're using a Map to to correctly serialize query keys that contain objects.
    const previousData = new Map()
    specificOptimisticUpdateDefinitions.forEach(({ queryKey, updateQuery }) => {
      // Snapshot the currently cached value.
      const previousDataForQuery = queryClient.getQueryData(queryKey)

      // Attempt to optimistically update the cache using the new value.
      try {
        queryClient.setQueryData(queryKey, updateQuery)
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
 * @param {InternalOptimisticUpdateDefinition} optimisticUpdateDefinition The general, "uninstantiated" optimistic
 * update definition with a function for constructing the query key.
 * @param item The item triggering the Action/optimistic update (i.e., the argument passed to the Action).
 * @returns {SpecificOptimisticUpdateDefinition} A specific optimistic update definition 
 * which corresponds to the provided definition and closes over the provided item.
 */
function getOptimisticUpdateDefinitionForSpecificItem(optimisticUpdateDefinition, item) {
  const { getQueryKey, updateQuery } = optimisticUpdateDefinition
  return {
    queryKey: getQueryKey(item),
    updateQuery: (old) => updateQuery(item, old)
  }
}

/**
 * Translates a Wasp query specifier to a query cache key used by React Query.
 * 
 * @param {QuerySpecifier} querySpecifier A query specifier that's a part of the public API:
 * https://wasp-lang.dev/docs/language/features#the-useaction-hook.
 * @returns {QueryKey} A cache key React Query internally uses for addressing queries.
 */
function getRqQueryKeyFromSpecifier(querySpecifier) {
  const [queryFn, ...otherKeys] = querySpecifier
  return [queryFn.queryCacheKey, ...otherKeys]
}
