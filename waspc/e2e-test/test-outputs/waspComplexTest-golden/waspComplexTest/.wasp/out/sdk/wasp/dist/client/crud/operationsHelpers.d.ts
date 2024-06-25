import { useAction, useQuery } from "../operations";
export declare function makeUseQueryFor<Input, Output>(query: Parameters<typeof useQuery<Input, Output>>[0]): (queryFnArgs?: Input, options?: any) => import("@tanstack/react-query").UseQueryResult<Output, Error>;
export declare function makeUseActionFor<Input = unknown, Output = unknown>(action: Parameters<typeof useAction<Input, Output>>[0]): (actionOptions?: {
    optimisticUpdates: import("../operations").OptimisticUpdateDefinition<Input, any>[];
}) => [Input] extends [never] ? (args?: unknown) => Promise<Output> : [Input] extends [void] ? () => Promise<Output> : (args: Input) => Promise<Output>;
