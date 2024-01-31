import { callOperation, makeOperationRoute } from '../internal/index.js'
import {
  addResourcesUsedByQuery,
  getActiveOptimisticUpdates,
} from '../internal/resources'

// PRIVATE API
export function createQuery(relativeQueryPath, entitiesUsed) {
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
  query,
  { relativeQueryPath, queryRoute, entitiesUsed }
) {
  query.queryCacheKey = [relativeQueryPath]
  query.route = queryRoute
  addResourcesUsedByQuery(query.queryCacheKey, entitiesUsed)
}
