import { Route } from 'wasp/client'
import type { Expand, _Awaited, _ReturnType } from 'wasp/universal/types'
import { type Query } from '../core.js'
import { callOperation, makeOperationRoute } from '../internal/index.js'
import {
  addResourcesUsedByQuery,
  getActiveOptimisticUpdates,
} from '../internal/resources'

export function createQuery<BackendQuery extends GenericBackendQuery>(
  relativeQueryPath: string,
  entitiesUsed: string[]
): QueryFor<BackendQuery> {
  const queryRoute = makeOperationRoute(relativeQueryPath)

  async function query(queryKey, queryArgs) {
    const serverResult = await callOperation(queryRoute, queryArgs)
    return getActiveOptimisticUpdates(queryKey).reduce(
      (result, update) => update(result),
      serverResult,
    )
  }

  addMetadataToQuery(query, { relativeQueryPath, queryRoute, entitiesUsed })

  return query
}

// PRIVATE API
export function addMetadataToQuery(
  query: (...args: any[]) => Promise<unknown>,
  metadata: {
    relativeQueryPath: string
    queryRoute: Route
    entitiesUsed: string[]
  }
): void

// PRIVATE API
export function addMetadataToQuery(
  query,
  { relativeQueryPath, queryRoute, entitiesUsed }
) {
  query.queryCacheKey = [relativeQueryPath]
  query.route = queryRoute
  addResourcesUsedByQuery(query.queryCacheKey, entitiesUsed)
}

export type QueryFor<BackendQuery extends GenericBackendQuery> = 
  Query<Parameters<BackendQuery>[0], _Awaited<_ReturnType<BackendQuery>>>


type GenericBackendQuery = (args: never, context: any) => unknown