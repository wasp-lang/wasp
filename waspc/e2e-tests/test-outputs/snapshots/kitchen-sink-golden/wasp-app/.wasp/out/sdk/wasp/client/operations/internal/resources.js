import { queryClientInitialized } from '../queryClient.js'
import { makeUpdateHandlersMap } from './updateHandlersMap'
import { hashQueryKey } from '@tanstack/react-query'

// Map where key is resource name and value is Set
// containing query ids of all the queries that use
// that resource.
const resourceToQueryCacheKeys = new Map()

const updateHandlers = makeUpdateHandlersMap(hashQueryKey)

// PRIVATE API
/**
 * Remembers that specified query is using specified resources.
 * If called multiple times for same query, resources are added, not reset.
 * @param {string[]} queryCacheKey - Unique key under used to identify query in the cache.
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

export function registerActionInProgress(optimisticUpdateTuples) {
  optimisticUpdateTuples.forEach(
    ({ queryKey, updateQuery }) => updateHandlers.add(queryKey, updateQuery)
  )
}

export async function registerActionDone(resources, optimisticUpdateTuples) {
  optimisticUpdateTuples.forEach(({ queryKey }) => updateHandlers.remove(queryKey))
  await invalidateQueriesUsing(resources)
}

export function getActiveOptimisticUpdates(queryKey) {
  return updateHandlers.getUpdateHandlers(queryKey)
}

export async function invalidateAndRemoveQueries() {
  const queryClient = await queryClientInitialized
  // If we don't reset the queries before removing them, Wasp will stay on
  // the same page. The user would have to manually refresh the page to "finish"
  // logging out.
  // When a query is removed, the `Observer` is removed as well, and the components
  // that are using the query are not re-rendered. This is why we need to reset
  // the queries, so that the `Observer` is re-created and the components are re-rendered.
  // For more details: https://github.com/wasp-lang/wasp/pull/1014/files#r1111862125
  queryClient.resetQueries()
  // If we don't remove the queries after invalidating them, the old query data
  // remains in the cache, casuing a potential privacy issue.
  queryClient.removeQueries()
}

/**
 * Invalidates all queries that are using specified resources.
 * @param {string[]} resources - Names of resources.
 */
async function invalidateQueriesUsing(resources) {
  const queryClient = await queryClientInitialized

  const queryCacheKeysToInvalidate = getQueriesUsingResources(resources)
  queryCacheKeysToInvalidate.forEach(
    queryCacheKey => queryClient.invalidateQueries(queryCacheKey)
  )
}

/**
 * @param {string} resource - Resource name.
 * @returns {string[]} Array of "query cache keys" of queries that use specified resource.
 */
function getQueriesUsingResource(resource) {
  return Array.from(resourceToQueryCacheKeys.get(resource) || [])
}

function getQueriesUsingResources(resources) {
  return Array.from(new Set(resources.flatMap(getQueriesUsingResource)))
}
