import { type Route } from "wasp/client";
import type { IfAny, _Awaited, _ReturnType } from "wasp/universal/types";
/**
 * The client Query object type. It's a callable Query function with some extra
 * properties (metadata).
 */
export type Query<Input, Output> = QueryFunction<Input, Output> & QueryMetadata;
/**
 * The client Action object type (unlike a Query, it's just a normal function).
 */
export type Action<Input, Output> = ClientOperation<Input, Output>;
/**
 * The client Query function type.
 */
export type QueryFunction<Input, Output> = ClientOperation<Input, Output>;
/**
 *  All extra properties (metadata) found on a Query object type.
 */
export type QueryMetadata = {
    queryCacheKey: string[];
    route: Route;
};
/**
 * Constructs the client RPC function type from the type of the operation's
 * definition on the backend.
 */
export type OperationRpcFor<BackendOperation extends GenericBackendOperation> = Parameters<BackendOperation> extends [] ? ClientOperation<void, _Awaited<_ReturnType<BackendOperation>>> : ClientOperation<Parameters<BackendOperation>[0], _Awaited<_ReturnType<BackendOperation>>>;
/**
 * A supertype of all possible backend operation definitions (i.e., Queries and
 * Actions)
 */
export type GenericBackendOperation = (args: never, context: any) => unknown;
/**
 * A supertype of all possible frontend RPC function types.
 */
export type GenericOperationRpc = (args: never) => Promise<unknown>;
type ClientOperation<Input, Output> = IfAny<Input, (args?: any) => Promise<Output>, ClientOperationWithNonAnyInput<Input, Output>>;
type ClientOperationWithNonAnyInput<Input, Output> = [
    Input
] extends [never] ? (args?: unknown) => Promise<Output> : [Input] extends [void] ? () => Promise<Output> : (args: Input) => Promise<Output>;
export {};
//# sourceMappingURL=rpc.d.ts.map