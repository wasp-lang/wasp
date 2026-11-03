import { hashQueryKey, type QueryKey } from "@tanstack/react-query";
import type { SpecificOptimisticUpdateDefinition } from "../hooks.js";
import { queryClientInitialized } from "../queryClient.js";
import { makeUpdateHandlersMap } from "./updateHandlersMap.js";

// Map where key is resource name and value is Set
// containing query ids of all the queries that use
// that resource.
const resourceToQueryCacheKeys = new Map<string, Set<QueryKey>>();

const updateHandlers = makeUpdateHandlersMap(hashQueryKey);

/**
 * Remembers that specified query is using specified resources.
 * If called multiple times for same query, resources are added, not reset.
 * @param queryCacheKey - Unique key under used to identify query in the cache.
 * @param resources - Names of resources that query is using.
 */
export function addResourcesUsedByQuery(
  queryCacheKey: QueryKey,
  resources: string[],
): void {
  for (const resource of resources) {
    let cacheKeys = resourceToQueryCacheKeys.get(resource);
    if (!cacheKeys) {
      cacheKeys = new Set();
      resourceToQueryCacheKeys.set(resource, cacheKeys);
    }
    cacheKeys.add(queryCacheKey);
  }
}

export function registerActionInProgress(
  optimisticUpdateTuples: SpecificOptimisticUpdateDefinition<any>[],
): void {
  optimisticUpdateTuples.forEach(({ queryKey, updateQuery }) =>
    updateHandlers.add(queryKey, updateQuery),
  );
}

export async function registerActionDone(
  resources: string[],
  optimisticUpdateTuples: SpecificOptimisticUpdateDefinition<any>[],
): Promise<void> {
  optimisticUpdateTuples.forEach(({ queryKey }) =>
    updateHandlers.remove(queryKey),
  );
  await invalidateQueriesUsing(resources);
}

export function getActiveOptimisticUpdates(
  queryKey: QueryKey,
): SpecificOptimisticUpdateDefinition<any>["updateQuery"][] {
  return updateHandlers.getUpdateHandlers(queryKey);
}

export async function invalidateAndRemoveQueries(): Promise<void> {
  const queryClient = await queryClientInitialized;
  // If we don't reset the queries before removing them, Wasp will stay on
  // the same page. The user would have to manually refresh the page to "finish"
  // logging out.
  // When a query is removed, the `Observer` is removed as well, and the components
  // that are using the query are not re-rendered. This is why we need to reset
  // the queries, so that the `Observer` is re-created and the components are re-rendered.
  // For more details: https://github.com/wasp-lang/wasp/pull/1014/files#r1111862125
  queryClient.resetQueries();
  // If we don't remove the queries after invalidating them, the old query data
  // remains in the cache, casuing a potential privacy issue.
  queryClient.removeQueries();
}

/**
 * Invalidates all queries that are using specified resources.
 * @param resources - Names of resources.
 */
async function invalidateQueriesUsing(resources: string[]) {
  const queryClient = await queryClientInitialized;

  const queryCacheKeysToInvalidate = getQueriesUsingResources(resources);
  await Promise.all(
    queryCacheKeysToInvalidate.map((queryCacheKey) =>
      queryClient.invalidateQueries(queryCacheKey),
    ),
  );
}

/**
 * @param resource - Resource name.
 * @returns Array of "query cache keys" of queries that use specified resource.
 */
function getQueriesUsingResource(resource: string) {
  return Array.from(resourceToQueryCacheKeys.get(resource) || []);
}

function getQueriesUsingResources(resources: string[]) {
  return Array.from(new Set(resources.flatMap(getQueriesUsingResource)));
}
