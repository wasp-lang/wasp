import type { Query, Action } from "../operations/rpc";
export declare function makeUseQueryFor<Input, Output>(query: Query<Input, Output>): (queryFnArgs?: Input, options?: any) => import("@tanstack/react-query").UseQueryResult<Output, Error>;
export declare function makeUseActionFor<Input = unknown, Output = unknown>(action: Action<Input, Output>): (actionOptions?: {
    optimisticUpdates: import("../operations").OptimisticUpdateDefinition<Input, any>[];
}) => [Input] extends [never] ? (args?: unknown) => Promise<Output> : [Input] extends [void] ? () => Promise<Output> : (args: Input) => Promise<Output>;
