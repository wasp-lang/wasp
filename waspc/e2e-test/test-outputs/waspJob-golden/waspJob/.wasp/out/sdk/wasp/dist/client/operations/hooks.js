import { useMutation, useQueryClient, useQuery as rqUseQuery, } from "@tanstack/react-query";
import { makeQueryCacheKey } from "./queries/core";
export { configureQueryClient } from "./queryClient";
// PUBLIC API
export function useQuery(query, queryFnArgs, options) {
    if (typeof query !== 'function') {
        throw new TypeError('useQuery requires queryFn to be a function.');
    }
    if (!query.queryCacheKey) {
        throw new TypeError('queryFn needs to have queryCacheKey property defined.');
    }
    return rqUseQuery({
        // todo: The full queryCacheKey is constructed in two places, both here and
        // inside the Query. See https://github.com/wasp-lang/wasp/issues/2017
        // FIXME: query fns don't handle the `undefined` case correctly
        // https://github.com/wasp-lang/wasp/issues/2017
        queryKey: makeQueryCacheKey(query, queryFnArgs),
        // FIXME: query fns don't handle the `undefined` case correctly
        // https://github.com/wasp-lang/wasp/issues/2017
        queryFn: () => query(queryFnArgs),
        ...options,
    });
}
// PUBLIC API
/**
 * A hook for adding extra behavior to a Wasp Action (e.g., optimistic updates).
 *
 * @param actionFn The Wasp Action you wish to enhance/decorate.
 * @param actionOptions An options object for enhancing/decorating the given Action.
 * @returns A decorated Action with added behavior but an unchanged API.
 */
export function useAction(actionFn, actionOptions) {
    const queryClient = useQueryClient();
    let mutationFn = actionFn;
    let options = {};
    if (actionOptions?.optimisticUpdates) {
        const optimisticUpdatesDefinitions = actionOptions.optimisticUpdates.map(translateToInternalDefinition);
        mutationFn = makeOptimisticUpdateMutationFn(actionFn, optimisticUpdatesDefinitions);
        options = makeRqOptimisticUpdateOptions(queryClient, optimisticUpdatesDefinitions);
    }
    // NOTE: We decided to hide React Query's extra mutation features (e.g.,
    // isLoading, onSuccess and onError callbacks, synchronous mutate) and only
    // expose a simple async function whose API matches the original Action.
    // We did this to avoid cluttering the API with stuff we're not sure we need
    // yet (e.g., isLoading), to postpone the action vs mutation dilemma, and to
    // clearly separate our opinionated API from React Query's lower-level
    // advanced API (which users can also use)
    const mutation = useMutation(mutationFn, options);
    // This assertion is necessary because, when the Input is void, we want to
    // present the function as not accepting a payload (which isn't consistent
    // with how it's defined).
    return ((args) => mutation.mutateAsync(args));
}
/**
 * Translates/Desugars a public optimistic update definition object into a
 * definition object our system uses internally.
 *
 * @param publicOptimisticUpdateDefinition An optimistic update definition
 * object that's a part of the public API:
 * https://wasp.sh/docs/language/features#the-useaction-hook.
 * @returns An internally-used optimistic update definition object.
 */
function translateToInternalDefinition(publicOptimisticUpdateDefinition) {
    const { getQuerySpecifier, updateQuery } = publicOptimisticUpdateDefinition;
    const definitionErrors = [];
    if (typeof getQuerySpecifier !== "function") {
        definitionErrors.push("`getQuerySpecifier` is not a function.");
    }
    if (typeof updateQuery !== "function") {
        definitionErrors.push("`updateQuery` is not a function.");
    }
    if (definitionErrors.length) {
        throw new TypeError(`Invalid optimistic update definition: ${definitionErrors.join(", ")}.`);
    }
    return {
        getQueryKey: (item) => getRqQueryKeyFromSpecifier(getQuerySpecifier(item)),
        updateQuery,
    };
}
/**
 * Creates a function that performs an action while telling it about the
 * optimistic updates it caused.
 *
 * @param actionFn The Wasp Action.
 * @param optimisticUpdateDefinitions The optimisitc updates the action causes.
 * @returns An decorated action which performs optimistic updates.
 */
function makeOptimisticUpdateMutationFn(actionFn, optimisticUpdateDefinitions) {
    return (function performActionWithOptimisticUpdates(item) {
        const specificOptimisticUpdateDefinitions = optimisticUpdateDefinitions.map((generalDefinition) => getOptimisticUpdateDefinitionForSpecificItem(generalDefinition, item));
        return actionFn.internal(item, specificOptimisticUpdateDefinitions);
        // This assertion is necessary because, when the Input is void, we want to
        // present the function as not accepting a payload (which isn't consistent
        // with how it's defined).
    });
}
/**
 * Given a ReactQuery query client and our internal definition of optimistic
 * updates, this function constructs an object describing those same optimistic
 * updates in a format we can pass into React Query's useMutation hook. In other
 * words, it translates our optimistic updates definition into React Query's
 * optimistic updates definition. Check their docs for details:
 * https://tanstack.com/query/v4/docs/guides/optimistic-updates?from=reactQueryV3&original=https://react-query-v3.tanstack.com/guides/optimistic-updates
 *
 * @param queryClient The QueryClient instance used by React Query.
 * @param optimisticUpdateDefinitions A list containing internal optimistic
 * updates definition objects (i.e., a list where each object carries the
 * instructions for performing particular optimistic update).
 * @returns An object containing 'onMutate' and 'onError' functions
 * corresponding to the given optimistic update definitions (check the docs
 * linked above for details).
 */
function makeRqOptimisticUpdateOptions(queryClient, optimisticUpdateDefinitions) {
    async function onMutate(item) {
        const specificOptimisticUpdateDefinitions = optimisticUpdateDefinitions.map((generalDefinition) => getOptimisticUpdateDefinitionForSpecificItem(generalDefinition, item));
        // Cancel any outgoing refetches (so they don't overwrite our optimistic update).
        // Theoretically, we can be a bit faster. Instead of awaiting the
        // cancellation of all queries, we could cancel and update them in parallel.
        // However, awaiting cancellation hasn't yet proven to be a performance bottleneck.
        await Promise.all(specificOptimisticUpdateDefinitions.map(({ queryKey }) => queryClient.cancelQueries(queryKey)));
        // We're using a Map to correctly serialize query keys that contain objects.
        const previousData = new Map();
        specificOptimisticUpdateDefinitions.forEach(({ queryKey, updateQuery }) => {
            // Snapshot the currently cached value.
            const previousDataForQuery = queryClient.getQueryData(queryKey);
            // Attempt to optimistically update the cache using the new value.
            try {
                queryClient.setQueryData(queryKey, updateQuery);
            }
            catch (e) {
                console.error("The `updateQuery` function threw an exception, skipping optimistic update:");
                console.error(e);
            }
            // Remember the snapshotted value to restore in case of an error.
            previousData.set(queryKey, previousDataForQuery);
        });
        return { previousData };
    }
    function onError(_err, _item, context) {
        // All we do in case of an error is roll back all optimistic updates. We ensure
        // not to do anything else because React Query rethrows the error. This allows
        // the programmer to handle the error as they usually would (i.e., we want the
        // error handling to work as it would if the programmer wasn't using optimistic
        // updates).
        context.previousData.forEach(async (data, queryKey) => {
            await queryClient.cancelQueries(queryKey);
            queryClient.setQueryData(queryKey, data);
        });
    }
    return {
        onMutate,
        onError,
    };
}
/**
 * Constructs the definition for optimistically updating a specific item. It
 * uses a closure over the updated item to construct an item-specific query key
 * (e.g., useful when the query key depends on an ID).
 *
 * @param optimisticUpdateDefinition The general, "uninstantiated" optimistic
 * update definition with a function for constructing the query key.
 * @param item The item triggering the Action/optimistic update (i.e., the
 * argument passed to the Action).
 * @returns A specific optimistic update definition which corresponds to the
 * provided definition and closes over the provided item.
 */
function getOptimisticUpdateDefinitionForSpecificItem(optimisticUpdateDefinition, item) {
    const { getQueryKey, updateQuery } = optimisticUpdateDefinition;
    return {
        queryKey: getQueryKey(item),
        updateQuery: (old) => updateQuery(item, old),
    };
}
// todo: Address the duplication between this function and the one in
// queries/core.ts. Details here: https://github.com/wasp-lang/wasp/issues/2017
/**
 * Translates a Wasp query specifier to a query cache key used by React Query.
 *
 * @param querySpecifier A query specifier that's a part of the public API:
 * https://wasp.sh/docs/language/features#the-useaction-hook.
 * @returns A cache key React Query internally uses for addressing queries.
 */
function getRqQueryKeyFromSpecifier(querySpecifier) {
    const [queryFn, ...otherKeys] = querySpecifier;
    return [...queryFn.queryCacheKey, ...otherKeys];
}
//# sourceMappingURL=hooks.js.map