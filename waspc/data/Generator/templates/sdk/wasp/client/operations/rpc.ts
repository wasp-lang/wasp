import { type Route } from "wasp/client";
import type {
  _Awaited,
  _ReturnType,
} from "wasp/universal/types"

// PRIVATE API (but should maybe be public, users define values of this type)
export type Query<Input, Output> = QueryFunction<Input, Output> & QueryMetadata

// PRIVATE API (but should maybe be public, users define values of this type)
export type Action<Input, Output> = ClientOperation<Input, Output>

// PRIVATE API
export type QueryFunction<Input, Output> = ClientOperation<Input, Output>

// PRIVATE API
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
export type OperationRpcFor<BackendOperation extends GenericBackendOperation> =
  Parameters<BackendOperation> extends []
    ? ClientOperation<void, _Awaited<_ReturnType<BackendOperation>>>
    : ClientOperation<
        Parameters<BackendOperation>[0],
        _Awaited<_ReturnType<BackendOperation>>
      >

// PRIVATE API (but should maybe be public, users use values of this type)
// Read this to understand the type: https://github.com/wasp-lang/wasp/pull/1090#discussion_r1159732471
export type ClientOperation<Input, Output> = [Input] extends [never]
  ? (args?: unknown) => Promise<Output>
  : (args: Input) => Promise<Output>;

// PRIVATE API (needed in SDK)
export type GenericBackendOperation = (args: never, context: any) => unknown
