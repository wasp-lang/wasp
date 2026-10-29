import type { Route } from "../../http.js";
import type {
  GenericBackendOperation,
  GenericOperationRpc,
  OperationRpcFor,
  Query,
  QueryMetadata,
} from "../../operations/rpc.js";
import { addResourcesUsedByQuery } from "./internal/resources.js";

// todo: find ways to remove this duplication and make the type more precise.
// Details here: https://github.com/wasp-lang/wasp/issues/2017
export function makeQueryCacheKey<Input, Output>(
  query: Query<Input, Output>,
  payload: Input,
): (string | Input)[] {
  return payload !== undefined
    ? [...query.queryCacheKey, payload]
    : query.queryCacheKey;
}

export function buildAndRegisterQuery<QF extends GenericOperationRpc>(
  queryFn: QF,
  {
    queryCacheKey,
    queryRoute,
    entitiesUsed,
  }: { queryCacheKey: string[]; queryRoute: Route; entitiesUsed: string[] },
): QueryForFunction<QF> {
  const query = queryFn as QueryForFunction<QF>;

  query.queryCacheKey = queryCacheKey;
  query.route = queryRoute;
  addResourcesUsedByQuery(query.queryCacheKey, entitiesUsed);

  return query;
}

/**
 * Constructs the client Query object type from the type of the Query's definition
 * on the backend.
 */
export type QueryFor<BackendQuery extends GenericBackendOperation> =
  QueryForFunction<QueryFunctionFor<BackendQuery>>;

/**
 * Constructs the client Query function type from the type of the Query's
 * definition on the backend.
 */
type QueryFunctionFor<BackendQuery extends GenericBackendOperation> =
  OperationRpcFor<BackendQuery>;

/**
 * Returns the appropriate client Query object type for the provided client
 * Query function type.
 */
type QueryForFunction<QF extends GenericOperationRpc> = QF & QueryMetadata;
