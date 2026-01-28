import { Route } from 'wasp/client'
import type { _Awaited, _ReturnType } from 'wasp/universal/types'
import type {
  GenericBackendOperation,
  GenericOperationRpc,
  OperationRpcFor,
  Query,
  QueryMetadata,
} from '../rpc.js'
import { callOperation, makeOperationRoute } from '../internal/index.js'
import {
  addResourcesUsedByQuery,
  getActiveOptimisticUpdates,
} from '../internal/resources'

// PRIVATE API (used in the SDK)
// todo: find ways to remove this duplication and make the type more precise.
// Details here: https://github.com/wasp-lang/wasp/issues/2017
export function makeQueryCacheKey<Input, Output>(
  query: Query<Input, Output>,
  payload: Input
): (string | Input)[] {
  return payload !== undefined ?
    [...query.queryCacheKey, payload]
    : query.queryCacheKey
}

// PRIVATE API (unsed in SDK)
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

// PRIVATE API (used in SDK)
export function buildAndRegisterQuery<QF extends GenericOperationRpc>(
  queryFn: QF,
  { queryCacheKey, queryRoute, entitiesUsed }:
    { queryCacheKey: string[], queryRoute: Route, entitiesUsed: string[] }
): QueryForFunction<QF> {
  const query = queryFn as QueryForFunction<QF>

  query.queryCacheKey = queryCacheKey
  query.route = queryRoute
  addResourcesUsedByQuery(query.queryCacheKey, entitiesUsed)

  return query
}

// PRIVATE API (but should maybe be public, users define values of this type)
/**
 * Constructs the client Query object type from the type of the Query's definition
 * on the backend.
 */
export type QueryFor<BackendQuery extends GenericBackendOperation> =
  QueryForFunction<QueryFunctionFor<BackendQuery>>

/**
 * Constructs the client Query function type from the type of the Query's
 * definition on the backend.
 */
type QueryFunctionFor<BackendQuery extends GenericBackendOperation> =
  OperationRpcFor<BackendQuery>

/**
 * Returns the appropriate client Query object type for the provided client
 * Query function type.
 */
type QueryForFunction<QF extends GenericOperationRpc> = QF & QueryMetadata
