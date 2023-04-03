import { UseQueryResult } from "@tanstack/react-query";

import { type HttpMethod } from "../types";

export type Query<Input, Output> = {
    (queryCacheKey: string[], args: Input): Promise<Output>
}

export function useQuery<Input, Output>(
    queryFn: Query<Input, Output>,
    queryFnArgs?: Input, options?: any
): UseQueryResult<Output, Error>
