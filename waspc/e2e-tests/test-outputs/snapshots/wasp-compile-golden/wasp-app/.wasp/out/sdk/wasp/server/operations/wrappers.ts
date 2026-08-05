import { IfAny, _Awaited, _ReturnType, _Parameters } from '../../universal/types'

import {
  _Entity,
  UnauthenticatedOperationDefinition,
  Payload,
} from '../_types'

// PRIVATE API (used in SDK)
// Explanation:
// - Custom `_Awaited` and `_ReturnType` - Read the comments above their
// definitions.
// - `Parameters<OperationDefinition> extends []` - Same reason as described here:
// https://github.com/wasp-lang/wasp/pull/1992/files#r1583040080
/**
 * Constructs the unauthenticated operation's server-side API type from its
 * definition.
 *
 * @template OperationDefinition The type of the unauthenticated operation's
 * definition.
 */
export type UnauthenticatedOperationFor<
  OperationDefinition extends GenericUnauthenticatedOperationDefinition
> = Parameters<OperationDefinition> extends []
  ? UnauthenticatedOperation<void, _Awaited<_ReturnType<OperationDefinition>>>
  : UnauthenticatedOperation<
      Parameters<OperationDefinition>[0],
      _Awaited<_ReturnType<OperationDefinition>>
    >

// PRIVATE API (used in SDK)
/**
 * Creates the server-side API for an unauthenticated operation.
 *
 * The operation definition is accepted through a getter instead of directly to
 * prevent "use before initialization" errors. When a user's operation definition
 * imports another operation's server-side API (i.e., imports from `"wasp/server/operations"`),
 * that module and ours form a cycle. Depending on which side of the cycle the
 * bundler enters first, it can emit our {@link createUnauthenticatedOperation}
 * wrapper calls above the user's operation definitions:
 * ```ts
 * function badCreateOperation(fn: Function) {
 *   return () => fn();
 * }
 * const badServerOperation = badCreateOperation(someUserOperation);
 *       ^! ReferenceError: Cannot access 'someUserOperation' before initialization
 * const someUserOperation = () => 1;
 * ```
 * 
 * The getter defers the read until the operation is called, by which point every
 * module is initialized:
 * ```ts
 * function goodCreateOperation(fn: () => Function) {
 *   return () => fn()();
 * }
 * const goodServerOperation = goodCreateOperation(() => someUserOperation);
 * const someUserOperation = () => 1;
 * ```
 * @template OperationDefinition The type of the unauthenticated operation's definition.
 * @param getUserOperation Returns the unauthenticated operation's definition.
 * @param entities The unauthenticated operation's entity map.
 * @returns The server-side API for the provided unauthenticated operation.
 */
export function createUnauthenticatedOperation<
  OperationDefinition extends GenericUnauthenticatedOperationDefinition
>(
  getUserOperation: () => OperationDefinition,
  entities: EntityMapFor<OperationDefinition>
): UnauthenticatedOperationFor<OperationDefinition> {
  async function operation(payload: Parameters<OperationDefinition>[0]) {
    return getUserOperation()(payload, {
      entities,
    })
  }
  // This assertion is necessary because, when the Input is void, we want to present
  // the function as not accepting a payload (which isn't consistent with how
  // it's defined).
  return operation as UnauthenticatedOperationFor<OperationDefinition>
}

// Read this to understand the type: https://github.com/wasp-lang/wasp/pull/2170#issue-2398830273
/**
 * Constructs the type for an unauthenticated operation's server-side API.
 *
 * @template Input The type of the payload the operation expects (must be
 * `void` if the operation doesn't expect a payload).
 * @template Output The type of the operation's return value.
 */
type UnauthenticatedOperation<Input, Output> =
  IfAny<
    Input,
    (args?: any) => Promise<Output>,
    UnauthenticatedOperationWithNonAnyInput<Input, Output>
  >

// Read this to understand the type: https://github.com/wasp-lang/wasp/pull/1090#discussion_r1159732471
type UnauthenticatedOperationWithNonAnyInput<Input, Output> =
  [Input] extends [never]
  ? (args?: unknown) => Promise<Output>
  : [Input] extends [void]
  ? () => Promise<Output>
  : (args: Input) => Promise<Output>

/**
 * The principal type for an unauthenticated operation's definition (i.e., all
 * unauthenticated operation definition types are a subtype of this type).
 *
 */
type GenericUnauthenticatedOperationDefinition = UnauthenticatedOperationDefinition<
  // NOTE(filip): Not quite sure I understand what's going on with Variance here.
  _Entity[],
  never,
  Payload
>

/**
 * Queries the entity map from the type of the operation's definition.
 *
 * @template OperationDefinition The type of the operation's definition.
 */
type EntityMapFor<OperationDefinition extends GenericUnauthenticatedOperationDefinition> =
  _Parameters<OperationDefinition>[1]["entities"]
