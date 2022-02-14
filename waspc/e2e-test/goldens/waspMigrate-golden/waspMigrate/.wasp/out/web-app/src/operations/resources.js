import queryCache from '../queryCache'


// Map where key is resource name and value is Set
// containing query ids of all the queries that use
// that resource.
const resourceToQueryCacheKeys = new Map()

/**
 * Remembers that specified query is using specified resources.
 * If called multiple times for same query, resources are added, not reset.
 * @param {string} queryCacheKey - Unique key under used to identify query in the cache.
 * @param {string[]} resources - Names of resources that query is using.
 */
export const addResourcesUsedByQuery = (queryCacheKey, resources) => {
  for (const resource of resources) {
    let cacheKeys = resourceToQueryCacheKeys.get(resource)
    if (!cacheKeys) {
      cacheKeys = new Set()
      resourceToQueryCacheKeys.set(resource, cacheKeys)
    }
    cacheKeys.add(queryCacheKey)
  }
}

/**
 * @param {string} resource - Resource name.
 * @returns {string[]} Array of "query cache keys" of queries that use specified resource.
 */
export const getQueriesUsingResource = (resource) => {
  return Array.from(resourceToQueryCacheKeys.get(resource) || [])
}

/**
 * Invalidates all queries that are using specified resources.
 * @param {string[]} resources - Names of resources.
 */
export const invalidateQueriesUsing = (resources) => {
  const queryCacheKeysToInvalidate = new Set()
  for (const resource of resources) {
    getQueriesUsingResource(resource).forEach(key => queryCacheKeysToInvalidate.add(key))
  }
  for (const queryCacheKey of queryCacheKeysToInvalidate) {
    queryCache.invalidateQueries(queryCacheKey)
  }
}
