import type { UseQueryResult } from "@tanstack/react-query"
import type { Tail, _Awaited, _ReturnType } from "../../universal/types"
import { useAction, useQuery } from "../operations"
import type { OptimisticUpdateDefinition } from "../operations/hooks"
import type { Action, GenericBackendOperation, OperationRpcFor, Query } from "../operations/rpc"

// PRIVATE API
export type UseQueryFor<BackendQuery extends GenericBackendOperation> =
  (queryFnArgs?: Parameters<BackendQuery>[0], options?: any) =>
    UseQueryResult<_Awaited<_ReturnType<BackendQuery>>, Error>

// PRIVATE API
export function makeUseQueryFor<Input, Output>(
  query: Query<Input, Output>
) {
  return (
    ...rest: Tail<Parameters<typeof useQuery<Input, Output>>>
  ) => useQuery<Input, Output>(query, ...rest);
}

// PRIVATE API
export type UseActionFor<BackendAction extends GenericBackendOperation> =
  (actionOptions?: {
    optimisticUpdates: OptimisticUpdateDefinition<Parameters<BackendAction>[0], any>[]
  }) => OperationRpcFor<BackendAction>

// PRIVATE API
export function makeUseActionFor<Input = unknown, Output = unknown>(
  action: Action<Input, Output>
) {
  return (
    ...rest: Tail<Parameters<typeof useAction<Input, Output>>>
  ) => useAction<Input, Output>(action, ...rest);
}
