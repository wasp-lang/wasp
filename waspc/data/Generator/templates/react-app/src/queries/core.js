import { callOperation } from '../operations'
import { addResourcesUsedByQuery } from '../operations/resources'

export function createQuery(queryRoute, entitiesUsed) {
  function query(args) {
    return callOperation(queryRoute, args)
  }

  query.queryCacheKey = queryRoute
  addResourcesUsedByQuery(query.queryCacheKey, entitiesUsed)

  return query
}
