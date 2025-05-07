import { UseQueryResult } from "@tanstack/react-query";
import { Action, Query } from "./rpc";
export { configureQueryClient } from "./queryClient";
export declare function useQuery<Input, Output>(query: Query<Input, Output>, queryFnArgs?: Input, options?: any): UseQueryResult<Output, Error>;
/**
 * A hook for adding extra behavior to a Wasp Action (e.g., optimistic updates).
 *
 * @param actionFn The Wasp Action you wish to enhance/decorate.
 * @param actionOptions An options object for enhancing/decorating the given Action.
 * @returns A decorated Action with added behavior but an unchanged API.
 */
export declare function useAction<Input = unknown, Output = unknown>(actionFn: Action<Input, Output>, actionOptions?: ActionOptions<Input>): typeof actionFn;
/**
 * A documented (public) way to define optimistic updates.
 */
export type OptimisticUpdateDefinition<ActionInput, CachedData> = {
    getQuerySpecifier: GetQuerySpecifier<ActionInput, CachedData>;
    updateQuery: UpdateQuery<ActionInput, CachedData>;
};
/**
 * An options object passed into the `useAction` hook and used to enhance the
 * action with extra options.
 *
 */
type ActionOptions<ActionInput> = {
    optimisticUpdates: OptimisticUpdateDefinition<ActionInput, any>[];
};
/**
 * A function that takes an item and returns a Wasp Query specifier.
 */
type GetQuerySpecifier<ActionInput, CachedData> = (item: ActionInput) => QuerySpecifier<unknown, CachedData>;
/**
 * A function that takes an item and the previous state of the cache, and returns
 * the desired (new) state of the cache.
 */
type UpdateQuery<ActionInput, CachedData> = (item: ActionInput, oldData: CachedData | undefined) => CachedData;
/**
 * A public query specifier used for addressing Wasp queries. See our docs for details:
 * https://wasp.sh/docs/language/features#the-useaction-hook.
 */
type QuerySpecifier<Input, Output> = [Query<Input, Output>, ...any[]];
//# sourceMappingURL=hooks.d.ts.map