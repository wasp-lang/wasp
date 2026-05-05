import { GenericOperationDefinition } from "../../server/_types";
import { OperationInput, OperationOutput } from "../../server/operations/wrappers";
import { useAction, useQuery } from "../operations";
import type { ActionFor } from "../operations/actions/core";
import type { ActionOptions } from "../operations/hooks";
import type { QueryFor } from "../operations/queries/core";

// PRIVATE API
export type UseQueryFor<Operation extends GenericOperationDefinition> = (
  queryFnArgs?: OperationInput<Operation>,
  options?: any
) => ReturnType<typeof useQuery<OperationInput<Operation>, OperationOutput<Operation>>>;

// PRIVATE API
export function makeUseQueryFor<Operation extends GenericOperationDefinition>(
  query: QueryFor<Operation>
): UseQueryFor<Operation> {
  return (queryFnArgs, options) => useQuery(query, queryFnArgs, options);
}

// PRIVATE API
export type UseActionFor<Operation extends GenericOperationDefinition> = (
  actionOptions?: ActionOptions<OperationInput<Operation>>
) => ReturnType<typeof useAction<OperationInput<Operation>, OperationOutput<Operation>>>;

// PRIVATE API
export function makeUseActionFor<Operation extends GenericOperationDefinition>(
  action: ActionFor<Operation>
): UseActionFor<Operation> {
  return (actionOptions) => useAction(action, actionOptions);
}

