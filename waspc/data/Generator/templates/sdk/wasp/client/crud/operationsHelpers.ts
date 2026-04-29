import type { UseQueryResult } from "@tanstack/react-query";
import type { _Awaited, _ReturnType } from "../../universal/types";
import { useAction, useQuery } from "../operations";
import type { ActionFor } from "../operations/actions/core";
import type { ActionOptions } from "../operations/hooks";
import type { QueryFor } from "../operations/queries/core";
import type { GenericBackendOperation } from "../operations/rpc";

// PRIVATE API
export type UseQueryFor<BackendQuery extends GenericBackendOperation> = (
  queryFnArgs?: Parameters<BackendQuery>[0],
  options?: any
) => UseQueryResult<_Awaited<_ReturnType<BackendQuery>>, Error>

// PRIVATE API
export function makeUseQueryFor<BackendQuery extends GenericBackendOperation>(
  query: QueryFor<BackendQuery>
): UseQueryFor<BackendQuery> {
  return ((queryFnArgs, options) =>
    useQuery(query, queryFnArgs, options)) as UseQueryFor<BackendQuery>
}

// PRIVATE API
export type UseActionFor<BackendAction extends GenericBackendOperation> = (
  actionOptions?: ActionOptions<Parameters<BackendAction>[0]>
) => ActionFor<BackendAction>

// PRIVATE API
export function makeUseActionFor<BackendAction extends GenericBackendOperation>(
  action: ActionFor<BackendAction>
): UseActionFor<BackendAction> {
  return ((actionOptions) =>
    useAction(action, actionOptions)) as UseActionFor<BackendAction>
}
