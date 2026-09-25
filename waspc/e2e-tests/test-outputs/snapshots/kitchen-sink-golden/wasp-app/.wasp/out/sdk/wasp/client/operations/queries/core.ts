import {
  makeQueryCacheKey,
  buildAndRegisterQuery,
  type QueryFor,
  addResourcesUsedByQuery,
  getActiveOptimisticUpdates,
} from '@wasp.sh/lib-sdk-core/browser'
export { buildAndRegisterQuery } from '@wasp.sh/lib-sdk-core/browser'
import {
  makeOperationRoute,
  type GenericBackendOperation,
  type OperationRpcFor,
} from '@wasp.sh/lib-sdk-core'
import { callOperation } from '../internal/index.js'

// PRIVATE API (used in SDK)
export function createQuery<BackendQuery extends GenericBackendOperation>(
  relativeQueryPath: string,
  entitiesUsed: string[]
): QueryFor<BackendQuery> {
  const queryRoute = makeOperationRoute(relativeQueryPath)
  const queryCacheKey = [relativeQueryPath]

  const queryFn = (async (queryArgs) => {
    const serverResult = await callOperation(queryRoute, queryArgs)
    // todo: The full queryCacheKey is constructed in two places, both here and
    // inside the useQuery hook. See
    // https://github.com/wasp-lang/wasp/issues/2017
    const queryCacheKey = makeQueryCacheKey(queryFn as QueryFor<BackendQuery>, queryArgs)
    return getActiveOptimisticUpdates(queryCacheKey).reduce(
      (result, update) => update(result),
      serverResult,
    )
    // This assertion is necessary because, when the Input is void, we want to
    // present the function as not accepting a payload (which isn't consistent
    // with how it's defined).
  }) as QueryFunctionFor<BackendQuery>

  return buildAndRegisterQuery(
    queryFn,
    { queryCacheKey, queryRoute, entitiesUsed },
  )
}

/**
 * Constructs the client Query function type from the type of the Query's
 * definition on the backend.
 */
type QueryFunctionFor<BackendQuery extends GenericBackendOperation> =
  OperationRpcFor<BackendQuery>
