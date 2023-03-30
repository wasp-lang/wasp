import { callOperation, makeOperationRoute } from '../operations'
import {
  addResourcesUsedByQuery,
  getActiveOptimisticUpdates,
} from '../operations/resources'

export function createQuery(relativeQueryRoute, entitiesUsed) {
  const queryRoute = makeOperationRoute(relativeQueryRoute)

  async function query(queryKey, queryArgs) {
    const serverResult = await callOperation(queryRoute, queryArgs)
    return getActiveOptimisticUpdates(queryKey).reduce(
      (result, update) => update(result), 
      serverResult,
    )
  }

  query.queryCacheKey = [relativeQueryRoute]
  query.route = queryRoute
  addResourcesUsedByQuery(query.queryCacheKey, entitiesUsed)

  return query
}
