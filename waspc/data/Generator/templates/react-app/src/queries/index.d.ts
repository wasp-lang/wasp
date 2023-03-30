import { UseQueryResult } from "@tanstack/react-query";

export type Query<Input, Output> = (queryKey: string[], args: Input) => Promise<Output>

export function useQuery<Input, Output>(
    queryFn: Query<Input, Output>,
    queryFnArgs?: Input, options?: any
): UseQueryResult<Output, Error>
