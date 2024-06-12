import { useAction, useQuery } from "wasp/client/operations";
export function makeUseQueryFor(query) {
    return (args, options) => useQuery(query, args, options);
}
export function makeUseActionFor(action) {
    return (actionOptions) => useAction(action, actionOptions);
}
//# sourceMappingURL=operationsHelpers.js.map