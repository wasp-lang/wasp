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
 * @template OperationDefinition The type of the unauthenticated operation's definition.
 * @param userOperation The unauthenticated operation's definition.
 * @param entities The unauthenticated operation's entity map .
 * @returns The server-side API for the provided unauthenticated operation.
 */
export function createUnauthenticatedOperation<
  OperationDefinition extends GenericUnauthenticatedOperationDefinition
>(
  userOperation: OperationDefinition,
  entities: EntityMapFor<OperationDefinition>
): UnauthenticatedOperationFor<OperationDefinition> {
  async function operation(payload: Parameters<OperationDefinition>[0]) {
    return userOperation(payload, {
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
