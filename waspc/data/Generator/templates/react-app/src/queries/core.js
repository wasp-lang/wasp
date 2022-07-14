import { callOperation } from '../operations'
import {
  addResourcesUsedByQuery,
  getPendingUpdatesForQuery
} from '../operations/resources'

export function createQuery(queryRoute, entitiesUsed) {
  async function query(queryKey, queryArgs) {
    const serverResult = await callOperation(queryRoute, queryArgs)
    return getPendingUpdatesForQuery(queryKey).reduce(
      (result, pendingUpdate) => pendingUpdate(result), 
      serverResult,
    )
  }

  query.queryCacheKey = queryRoute
  addResourcesUsedByQuery(query.queryCacheKey, entitiesUsed)

  return query
}
