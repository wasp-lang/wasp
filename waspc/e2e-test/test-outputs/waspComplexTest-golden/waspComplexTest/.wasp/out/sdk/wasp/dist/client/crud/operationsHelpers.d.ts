import { useAction, useQuery } from "../operations";
import type { Query, Action } from "../operations/rpc";
import type { Tail } from "../../universal/types";
export declare function makeUseQueryFor<Input, Output>(query: Query<Input, Output>): (queryFnArgs?: Input | undefined, options?: any) => import("@tanstack/react-query").UseQueryResult<Output, Error>;
export declare function makeUseActionFor<Input = unknown, Output = unknown>(action: Action<Input, Output>): (actionOptions?: {
    optimisticUpdates: import("../operations").OptimisticUpdateDefinition<Input, any>[];
} | undefined) => import("../../universal/types").IfAny<Input, (args?: any) => Promise<Output>, [Input] extends [never] ? (args?: unknown) => Promise<Output> : [Input] extends [void] ? () => Promise<Output> : (args: Input) => Promise<Output>>;
