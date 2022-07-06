{{= {= =} =}}
// todo: can i remove the template thing above ^
import { queryClientInitialized } from '../queryClient'
import { makeCounter } from './counter'


// Map where key is resource name and value is Set
// containing query ids of all the queries that use
// that resource.
const resourceToQueryCacheKeys = new Map()
const actionCounter = makeCounter()

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

function getQueryHashes(queryClient, optimisticallyUpdatedCacheKeys) {
  return optimisticallyUpdatedCacheKeys
    .map(queryCacheKey => getQueryHash(queryClient, queryCacheKey))
    // filter out non-existing queries
    // very ugly
    .filter(Boolean)
}

export async function registerActionInProgress(optimisticallyUpdatedCacheKeys) {
  const queryClient = await queryClientInitialized
  const queryHashes = await getQueryHashes(queryClient, optimisticallyUpdatedCacheKeys)
  queryHashes.forEach(queryHash => actionCounter.increment(queryHash))
}

function getQueryHash(queryClient, queryCacheKey) {
  return queryClient.getQueryCache().find(queryCacheKey, { exact: true })?.queryHash
}

export async function registerActionDone(resources, optimisticallyUpdatedCacheKeys) {
  const queryClient = await queryClientInitialized
  const queryHashes = await getQueryHashes(queryClient, optimisticallyUpdatedCacheKeys)
  queryHashes.forEach(queryHash => actionCounter.decrement(queryHash))
  await invalidateQueriesUsing(resources)
}

export async function removeQueries() {
  const queryClient = await queryClientInitialized
  queryClient.removeQueries()
}

export async function invalidateAndRemoveQueries() {
  const queryClient = await queryClientInitialized
  // If we don't invalidate the queries before removing them, Wasp will stay on
  // the same page. The user would have to manually refresh the page to "finish"
  // logging out.
  queryClient.invalidateQueries()
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
    .filter(queryCacheKey => hasNoActionsInProgress(queryClient, queryCacheKey))

  queryCacheKeysToInvalidate.forEach(queryCacheKey => {
    console.log('invalidating', queryCacheKey)
    queryClient.invalidateQueries(queryCacheKey)
  })
}

function hasNoActionsInProgress(queryClient, queryCacheKey) {
  const queryHash = getQueryHash(queryClient, queryCacheKey)
  return actionCounter.count(queryHash) === 0
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