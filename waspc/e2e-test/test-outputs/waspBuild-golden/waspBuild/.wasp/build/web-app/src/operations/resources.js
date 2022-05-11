import { queryClient } from '../queryClient'


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
export function addResourcesUsedByQuery(queryCacheKey, resources) {
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
export function getQueriesUsingResource(resource) {
  return Array.from(resourceToQueryCacheKeys.get(resource) || [])
}

/**
 * Invalidates all queries that are using specified resources.
 * @param {string[]} resources - Names of resources.
 */
export function invalidateQueriesUsing(resources) {
  const queryCacheKeysToInvalidate = new Set()
  for (const resource of resources) {
    getQueriesUsingResource(resource).forEach(key => queryCacheKeysToInvalidate.add(key))
  }
  for (const queryCacheKey of queryCacheKeysToInvalidate) {
    queryClient.invalidateQueries(queryCacheKey)
  }
}

export function removePrivateQueries() {
  // TODO(matija): Currently we are removing all the queries, but we should remove only
  // non-public, user-dependent queries - public queries are expected not to change in respect
  // to the currently logged in user.
  // TODO(filip): why remove? What does it do exactly: https://github.com/tannerlinsley/react-query/blob/master/src/core/queryClient.ts
  queryClient.removeQueries()
}

export function invalidateAndClearQueries() {
  // TODO(matija): We are currently invalidating all the queries, but we should clear only the
  // non-public, user-dependent ones.
  // TODO(filip): I don't think we should be invalidating anything here
  queryClient.invalidateQueries()

  // TODO(matija): We are currently clearing all the queries, but we should clear only the
  // non-public, user-dependent ones.
  // TODO(filip): why clear? What does it do exactly: https://github.com/tannerlinsley/react-query/blob/master/src/core/queryClient.ts
  queryClient.clear()
}
