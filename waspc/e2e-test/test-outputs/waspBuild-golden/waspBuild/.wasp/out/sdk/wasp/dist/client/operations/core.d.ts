import { UseQueryResult } from "@tanstack/react-query";
export { configureQueryClient } from "./queryClient";
export type Query<Input, Output> = {
    (queryCacheKey: string[], args: Input): Promise<Output>;
};
export declare function useQuery<Input, Output>(queryFn: Query<Input, Output>, queryFnArgs?: Input, options?: any): UseQueryResult<Output, Error>;
export type Action<Input, Output> = [Input] extends [never] ? (args?: unknown) => Promise<Output> : (args: Input) => Promise<Output>;
/**
 * An options object passed into the `useAction` hook and used to enhance the
 * action with extra options.
 *
 */
export type ActionOptions<ActionInput> = {
    optimisticUpdates: OptimisticUpdateDefinition<ActionInput, any>[];
};
/**
 * A documented (public) way to define optimistic updates.
 */
export type OptimisticUpdateDefinition<ActionInput, CachedData> = {
    getQuerySpecifier: GetQuerySpecifier<ActionInput, CachedData>;
    updateQuery: UpdateQuery<ActionInput, CachedData>;
};
/**
 * A function that takes an item and returns a Wasp Query specifier.
 */
export type GetQuerySpecifier<ActionInput, CachedData> = (item: ActionInput) => QuerySpecifier<unknown, CachedData>;
/**
 * A function that takes an item and the previous state of the cache, and returns
 * the desired (new) state of the cache.
 */
export type UpdateQuery<ActionInput, CachedData> = (item: ActionInput, oldData: CachedData | undefined) => CachedData;
/**
 * A public query specifier used for addressing Wasp queries. See our docs for details:
 * https://wasp-lang.dev/docs/language/features#the-useaction-hook.
 */
export type QuerySpecifier<Input, Output> = [Query<Input, Output>, ...any[]];
/**
 * A hook for adding extra behavior to a Wasp Action (e.g., optimistic updates).
 *
 * @param actionFn The Wasp Action you wish to enhance/decorate.
 * @param actionOptions An options object for enhancing/decorating the given Action.
 * @returns A decorated Action with added behavior but an unchanged API.
 */
export declare function useAction<Input = unknown, Output = unknown>(actionFn: Action<Input, Output>, actionOptions?: ActionOptions<Input>): typeof actionFn;
