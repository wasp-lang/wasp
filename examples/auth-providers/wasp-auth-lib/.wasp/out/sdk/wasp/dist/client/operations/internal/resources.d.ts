/**
 * Remembers that specified query is using specified resources.
 * If called multiple times for same query, resources are added, not reset.
 * @param {string[]} queryCacheKey - Unique key under used to identify query in the cache.
 * @param {string[]} resources - Names of resources that query is using.
 */
export function addResourcesUsedByQuery(queryCacheKey: string[], resources: string[]): void;
export function registerActionInProgress(optimisticUpdateTuples: any): void;
export function registerActionDone(resources: any, optimisticUpdateTuples: any): Promise<void>;
export function getActiveOptimisticUpdates(queryKey: any): any;
export function invalidateAndRemoveQueries(): Promise<void>;
//# sourceMappingURL=resources.d.ts.map