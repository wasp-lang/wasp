import { Route } from 'wasp/client'
import type { _Awaited, _ReturnType } from 'wasp/universal/types'
import type { 
  GenericBackendOperation,
  OperationRpcFor,
  QueryFunction,
  QueryMetadata,
} from '../rpc.js'
import { callOperation, makeOperationRoute } from '../internal/index.js'
import {
  addResourcesUsedByQuery,
  getActiveOptimisticUpdates,
} from '../internal/resources'

// PRIVATE API (unsed in SDK)
export function createQuery<BackendQuery extends GenericBackendOperation>(
  relativeQueryPath: string,
  entitiesUsed: string[]
): QueryFor<BackendQuery> {
  const queryRoute = makeOperationRoute(relativeQueryPath)
  const queryCacheKey = [relativeQueryPath]

  const queryFn: QueryFunctionFor<BackendQuery> = async (queryArgs) => { 
    const serverResult = await callOperation(queryRoute, queryArgs)
    return getActiveOptimisticUpdates(queryCacheKey).reduce(
      (result, update) => update(result),
      serverResult,
    )
  }

  return buildAndRegisterQuery(
    queryFn,
    { queryCacheKey, queryRoute, entitiesUsed },
  )
}

// PRIVATE API (used in SDK)
export function buildAndRegisterQuery<Input, Output>(
  queryFn: QueryFunction<Input, Output>,
  { queryCacheKey, queryRoute, entitiesUsed }: 
  { queryCacheKey: string[], queryRoute: Route, entitiesUsed: string[] }
): QueryForFunction<typeof queryFn> {
  const query = queryFn as QueryForFunction<typeof queryFn>

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
type QueryForFunction<QF extends QueryFunction<never, unknown>> = 
  QF & QueryMetadata
