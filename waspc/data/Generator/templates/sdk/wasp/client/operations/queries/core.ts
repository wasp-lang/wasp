import { Route } from 'wasp/client'
import type { _Awaited, _ReturnType } from 'wasp/universal/types'
import type { QueryFunction, QueryForFunction } from '../core.js'
import { callOperation, makeOperationRoute } from '../internal/index.js'
import {
  addResourcesUsedByQuery,
  getActiveOptimisticUpdates,
} from '../internal/resources'

// PRIVATE API (unsed in SDK)
export function createQuery<BackendQuery extends GenericBackendQuery>(
  relativeQueryPath: string,
  entitiesUsed: string[]
): QueryFor<BackendQuery> {
  const queryRoute = makeOperationRoute(relativeQueryPath)
  const queryCacheKey = [relativeQueryPath]

  const query: QueryFunctionFor<BackendQuery> = (async (queryArgs) => { 
    const serverResult = await callOperation(queryRoute, queryArgs)
    return getActiveOptimisticUpdates(queryCacheKey).reduce(
      (result, update) => update(result),
      serverResult,
    )
  })

  addMetadataToQuery(query, { queryCacheKey, queryRoute, entitiesUsed })

  return query
}

// PRIVATE API (used in SDK)
export function addMetadataToQuery<Input, Output>(
  query: QueryFunction<Input, Output>,
  { queryCacheKey, queryRoute, entitiesUsed }: 
  { queryCacheKey: string[], queryRoute: Route, entitiesUsed: string[] }
): asserts query is QueryForFunction<typeof query> {
  const internalQuery = query as QueryForFunction<typeof query>

  internalQuery.queryCacheKey = queryCacheKey 
  internalQuery.route = queryRoute
  addResourcesUsedByQuery(internalQuery.queryCacheKey, entitiesUsed)
}

// PRIVATE API (but should maybe be public, users define values of this type)
export type QueryFor<BackendQuery extends GenericBackendQuery> =
  QueryForFunction<QueryFunctionFor<BackendQuery>>

export type QueryFunctionFor<BackendQuery extends GenericBackendQuery> =
  Parameters<BackendQuery> extends []
    ? QueryFunction<void, _Awaited<_ReturnType<BackendQuery>>>
    : QueryFunction<
        Parameters<BackendQuery>[0],
        _Awaited<_ReturnType<BackendQuery>>
      >

type GenericBackendQuery = (args: never, context: any) => unknown