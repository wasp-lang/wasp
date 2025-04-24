import { Route } from 'wasp/client';
import type { GenericBackendOperation, GenericOperationRpc, OperationRpcFor, Query, QueryMetadata } from '../rpc.js';
export declare function makeQueryCacheKey<Input, Output>(query: Query<Input, Output>, payload: Input): (string | Input)[];
export declare function createQuery<BackendQuery extends GenericBackendOperation>(relativeQueryPath: string, entitiesUsed: string[]): QueryFor<BackendQuery>;
export declare function buildAndRegisterQuery<QF extends GenericOperationRpc>(queryFn: QF, { queryCacheKey, queryRoute, entitiesUsed }: {
    queryCacheKey: string[];
    queryRoute: Route;
    entitiesUsed: string[];
}): QueryForFunction<QF>;
/**
 * Constructs the client Query object type from the type of the Query's definition
 * on the backend.
 */
export type QueryFor<BackendQuery extends GenericBackendOperation> = QueryForFunction<QueryFunctionFor<BackendQuery>>;
/**
 * Constructs the client Query function type from the type of the Query's
 * definition on the backend.
 */
type QueryFunctionFor<BackendQuery extends GenericBackendOperation> = OperationRpcFor<BackendQuery>;
/**
 * Returns the appropriate client Query object type for the provided client
 * Query function type.
 */
type QueryForFunction<QF extends GenericOperationRpc> = QF & QueryMetadata;
export {};
//# sourceMappingURL=core.d.ts.map