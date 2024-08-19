import { type Route } from "wasp/client";
import type {
  IfAny,
  _Awaited,
  _ReturnType,
} from "wasp/universal/types"

// PRIVATE API (for SDK, should maybe be public, users define values of this
// type).
//
// Frontend queries are functions with some extra properties (metadata).
// 
// To simplify working with the type (i.e., referencing the type's two different
// components), we've defined it as an intersection of two distinct types:
// one representing the function (QueryFunction), and the other representing the
// metadata (QueryMetadata) .
/**
 * The client Query object type. It's a callable Query function with some extra
 * properties (metadata).
 */
export type Query<Input, Output> = QueryFunction<Input, Output> & QueryMetadata

// PRIVATE API (for SDK, should maybe be public, users define values of this
// type)
/**
 * The client Action object type (unlike a Query, it's just a normal function).
 */
export type Action<Input, Output> = ClientOperation<Input, Output>

// PRIVATE API (for SDK)
/**
 * The client Query function type.
 */
export type QueryFunction<Input, Output> = ClientOperation<Input, Output>

// PRIVATE API (for SDK)
// todo: revisit what we expose. Also, queryCacheKey is confusing and
// incorrect.Details here: https://github.com/wasp-lang/wasp/issues/2017
/**
 *  All extra properties (metadata) found on a Query object type.
 */
export type QueryMetadata = {
  queryCacheKey: string[]
  route: Route
}

// PRIVATE API (needed in SDK)
// Explanation:
// - Custom `_Awaited` and `_ReturnType` - Read the comments above their
// definitions.
// - `Parameters<BackendOperation> extends []` - See here:
// https://github.com/wasp-lang/wasp/pull/1992/files#r1583040080
/**
 * Constructs the client RPC function type from the type of the operation's
 * definition on the backend.
 */
export type OperationRpcFor<BackendOperation extends GenericBackendOperation> =
  Parameters<BackendOperation> extends []
  ? ClientOperation<void, _Awaited<_ReturnType<BackendOperation>>>
  : ClientOperation<
    Parameters<BackendOperation>[0],
    _Awaited<_ReturnType<BackendOperation>>
  >

// PRIVATE API (needed in SDK)
/**
 * A supertype of all possible backend operation definitions (i.e., Queries and
 * Actions) 
 */
export type GenericBackendOperation = (args: never, context: any) => unknown

// PRIVATE API (needed in SDK)
/**
 * A supertype of all possible frontend RPC function types.
 */
export type GenericOperationRpc = (args: never) => Promise<unknown>

// NOTE: There's some duplication in the below types.
// Read the discussion here to understand why before trying to remove it:
// https://github.com/wasp-lang/wasp/pull/2170#discussion_r1671285049
//
// Read this to understand the type: https://github.com/wasp-lang/wasp/pull/2170#issue-2398830273
type ClientOperation<Input, Output> =
  IfAny<
    Input,
    (args?: any) => Promise<Output>,
    ClientOperationWithNonAnyInput<Input, Output>
  >

// Read this to understand the type: https://github.com/wasp-lang/wasp/pull/1090#discussion_r1159732471
type ClientOperationWithNonAnyInput<Input, Output> =
  [Input] extends [never]
  ? (args?: unknown) => Promise<Output>
  : [Input] extends [void]
  ? () => Promise<Output>
  : (args: Input) => Promise<Output>
