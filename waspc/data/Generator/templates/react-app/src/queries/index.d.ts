import { UseQueryResult } from "@tanstack/react-query";

import { type HttpMethod } from "../types";

export type Query<Input, Output> = {
    (args: Input): Promise<Output>
    queryCacheKey: string[]
    route: { method: HttpMethod, path: string }
}

export function useQuery<Input, Output>(
    queryFn: Query<Input, Output>,
    queryFnArgs?: Input, options?: any
): UseQueryResult<Output, Error>
