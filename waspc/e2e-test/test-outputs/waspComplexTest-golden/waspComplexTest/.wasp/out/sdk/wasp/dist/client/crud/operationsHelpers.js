import { useAction, useQuery } from "../operations";
// PRIVATE API
export function makeUseQueryFor(query) {
    return (...rest) => useQuery(query, ...rest);
}
// PRIVATE API
export function makeUseActionFor(action) {
    return (...rest) => useAction(action, ...rest);
}
//# sourceMappingURL=operationsHelpers.js.map