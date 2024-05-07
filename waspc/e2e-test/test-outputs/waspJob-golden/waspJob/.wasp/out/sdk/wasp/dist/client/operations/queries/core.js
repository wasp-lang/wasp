import { callOperation, makeOperationRoute } from '../internal/index.js';
import { addResourcesUsedByQuery, getActiveOptimisticUpdates, } from '../internal/resources';
// PRIVATE API (used in the SDK)
export function makeQueryCacheKey(query, payload) {
    return payload !== undefined ?
        [...query.queryCacheKey, payload]
        : query.queryCacheKey;
}
// PRIVATE API (unsed in SDK)
export function createQuery(relativeQueryPath, entitiesUsed) {
    const queryRoute = makeOperationRoute(relativeQueryPath);
    const queryCacheKey = [relativeQueryPath];
    const queryFn = async (queryArgs) => {
        const serverResult = await callOperation(queryRoute, queryArgs);
        const queryCacheKey = makeQueryCacheKey(queryFn, queryArgs);
        return getActiveOptimisticUpdates(queryCacheKey).reduce((result, update) => update(result), serverResult);
    };
    return buildAndRegisterQuery(queryFn, { queryCacheKey, queryRoute, entitiesUsed });
}
// PRIVATE API (used in SDK)
export function buildAndRegisterQuery(queryFn, { queryCacheKey, queryRoute, entitiesUsed }) {
    const query = queryFn;
    query.queryCacheKey = queryCacheKey;
    query.route = queryRoute;
    addResourcesUsedByQuery(query.queryCacheKey, entitiesUsed);
    return query;
}
//# sourceMappingURL=core.js.map