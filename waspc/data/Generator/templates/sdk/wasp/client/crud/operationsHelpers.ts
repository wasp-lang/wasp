import type { _Awaited, _ReturnType } from "../../universal/types";
import { useAction, useQuery } from "../operations";
import type { ActionFor } from "../operations/actions/core";
import type { ActionOptions } from "../operations/hooks";
import type { QueryFor } from "../operations/queries/core";
import type { GenericBackendOperation } from "../operations/rpc";

// PRIVATE API
export type UseQueryFor<Operation extends GenericBackendOperation> = (
  queryFnArgs?: OperationInput<Operation>,
  options?: any
) => ReturnType<typeof useQuery<OperationInput<Operation>, OperationOutput<Operation>>>;

// PRIVATE API
export function makeUseQueryFor<Operation extends GenericBackendOperation>(
  query: QueryFor<Operation>
): UseQueryFor<Operation> {
  return (queryFnArgs, options) => useQuery(query, queryFnArgs, options);
}

// PRIVATE API
export type UseActionFor<Operation extends GenericBackendOperation> = (
  actionOptions?: ActionOptions<OperationInput<Operation>>
) => ReturnType<typeof useAction<OperationInput<Operation>, OperationOutput<Operation>>>;

// PRIVATE API
export function makeUseActionFor<Operation extends GenericBackendOperation>(
  action: ActionFor<Operation>
): UseActionFor<Operation> {
  return (actionOptions) => useAction(action, actionOptions);
}

type OperationInput<Operation extends GenericBackendOperation> = Parameters<Operation>[0];
type OperationOutput<Operation extends GenericBackendOperation> = _Awaited<_ReturnType<Operation>>
