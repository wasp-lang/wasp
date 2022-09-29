import { callOperation } from '../operations'
import {
  addResourcesUsedByQuery,
  getActiveOptimisticUpdates,
} from '../operations/resources'

export function createQuery(queryRoute, entitiesUsed) {
  async function query(queryKey, queryArgs) {
    const serverResult = await callOperation(queryRoute, queryArgs)
    return getActiveOptimisticUpdates(queryKey).reduce(
      (result, update) => update(result), 
      serverResult,
    )
  }

  query.queryCacheKey = queryRoute
  addResourcesUsedByQuery(query.queryCacheKey, entitiesUsed)

  return query
}
