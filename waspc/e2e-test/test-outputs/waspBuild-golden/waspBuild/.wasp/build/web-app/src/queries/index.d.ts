import { UseQueryResult } from "@tanstack/react-query";

export type Query<Input, Output> = (args: Input) => Promise<Output>

export function useQuery<Input, Output, Error = unknown>(
    queryFn: Query<Input, Output>,
    queryFnArgs?: Input, options?: any
): UseQueryResult<Output, Error>
