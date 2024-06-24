import { callOperation, makeOperationRoute } from '../internal/index.js';
import { addResourcesUsedByQuery, getActiveOptimisticUpdates, } from '../internal/resources';
// PRIVATE API (used in the SDK)
// todo: find ways to remove this duplication and make the type more precise.
// Details here: https://github.com/wasp-lang/wasp/issues/2017
export function makeQueryCacheKey(query, payload) {
    return payload !== undefined ?
        [...query.queryCacheKey, payload]
        : query.queryCacheKey;
}
// PRIVATE API (unsed in SDK)
export function createQuery(relativeQueryPath, entitiesUsed) {
    const queryRoute = makeOperationRoute(relativeQueryPath);
    const queryCacheKey = [relativeQueryPath];
    const queryFn = (async (queryArgs) => {
        const serverResult = await callOperation(queryRoute, queryArgs);
        // todo: The full queryCacheKey is constructed in two places, both here and
        // inside the useQuery hook. See
        // https://github.com/wasp-lang/wasp/issues/2017
        const queryCacheKey = makeQueryCacheKey(queryFn, queryArgs);
        return getActiveOptimisticUpdates(queryCacheKey).reduce((result, update) => update(result), serverResult);
        // This assertion is necessary because, when the Input is void, we want to
        // present the function as not accepting a payload (which isn't consistent
        // with how it's defined).
    });
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