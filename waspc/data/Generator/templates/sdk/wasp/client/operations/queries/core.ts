import { Route } from 'wasp/client'
import type { _Awaited, _ReturnType } from 'wasp/universal/types'
import type { Query, InternalViewOf } from '../core.js'
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

  type Q = QueryFor<BackendQuery>
  const query: Q = async (queryArgs) => { 
    // Assumes `addMetadataToQuery` added the `queryCacheKey` property to the query.
    const queryKey = (query as InternalViewOf<Q>).queryCacheKey
    const serverResult = await callOperation(queryRoute, queryArgs)
    return getActiveOptimisticUpdates(queryKey).reduce(
      (result, update) => update(result),
      serverResult,
    )
  }

  addMetadataToQuery(query, { relativeQueryPath, queryRoute, entitiesUsed })

  return query
}

// PRIVATE API (used in SDK)
export function addMetadataToQuery<Input, Output>(
  query: Query<Input, Output>,
  { relativeQueryPath, queryRoute, entitiesUsed }: 
  { relativeQueryPath: string, queryRoute: Route, entitiesUsed: string[] }
): asserts query is InternalViewOf<typeof query> {
  const internalQuery = query as InternalViewOf<typeof query>

  internalQuery.queryCacheKey = [relativeQueryPath]
  internalQuery.route = queryRoute
  addResourcesUsedByQuery(internalQuery.queryCacheKey, entitiesUsed)
}

export type QueryFor<BackendQuery extends GenericBackendQuery> =
  Parameters<BackendQuery> extends []
    ? Query<void, _Awaited<_ReturnType<BackendQuery>>>
    : Query<Parameters<BackendQuery>[0], _Awaited<_ReturnType<BackendQuery>>>

type GenericBackendQuery = (args: never, context: any) => unknown

