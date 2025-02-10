import { IfAny, _Awaited, _ReturnType, _Parameters } from '../../universal/types'

import { type AuthUser } from 'wasp/auth'
import {
  _Entity,
  AuthenticatedOperationDefinition,
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

// PRIVATE API (used in SDK)
// Explanation:
// - Custom `_Awaited` and `_ReturnType` - Read the comments above their
// definitions.
// - `Parameters<OperationDefinition> extends []` - Same reason as described here:
// https://github.com/wasp-lang/wasp/pull/1992/files#r1583040080
/**
 * Constructs the authenticated operation's server-side API type from its
 * definition.
 *
 * @template OperationDefinition The type of the authenticated operation's
 * definition.
 */
export type AuthenticatedOperationFor<
  OperationDefinition extends GenericAuthenticatedOperationDefinition
> = Parameters<OperationDefinition> extends []
  ? AuthenticatedOperation<void, _Awaited<_ReturnType<OperationDefinition>>>
  : AuthenticatedOperation<
      Parameters<OperationDefinition>[0],
      _Awaited<_ReturnType<OperationDefinition>>
    >

/**
 * The type of the context users must pass when calling an authenticated
 * operation's server-side API.
 */
export type AuthenticatedOperationContext = { user: AuthUser }

// PRIVATE API (used in SDK)
/**
 * Creates the server-side API for an authenticated operation.
 *
 * @template OperationDefinition The type of the authenticated operation's definition.
 * @param userOperation The authenticated operation's definition.
 * @param entities The authenticated operation's entity map .
 * @returns The server-side API for the provided authenticated operation.
 */
export function createAuthenticatedOperation<
  OperationDefinition extends GenericAuthenticatedOperationDefinition
>(
  userOperation: OperationDefinition,
  entities: EntityMapFor<OperationDefinition>
): AuthenticatedOperationFor<OperationDefinition> {
  async function operation(...args: AuthenticatedOperationArgsFor<OperationDefinition>) {
    // To understand this function and its limitations, read
    // https://github.com/wasp-lang/wasp/issues/2050
    if (args.length < 1) {
      // No arguments sent -> no user and no payload specified -> there's no way this was called correctly.
      throw new Error(`
        You called the operation without arguments, which is a mistake.
        Check your definition and read the docs to understand what you need to send:
        https://wasp.sh/docs/data-model/operations/overview
        `
      )
    } else if (includesPayload(args)) {
      // Two arguments sent -> the first argument is the payload, the second is the context.
      const [payload, context] = args
      return userOperation(payload as Parameters<OperationDefinition>[0], {
        ...context,
        entities,
      })
    } else {
      // One argument sent -> the first and only argument is the user.
      const [context] = args
      return userOperation(undefined as Parameters<OperationDefinition>[0], {
        ...context,
        entities,
      })
    }
  }

  return operation as AuthenticatedOperationFor<OperationDefinition>
}

/**
 * Returns a boolean carrying compiler type information about whether the
 * provided arguments array (of an authenticated operation) includes a payload.
 *
 * To understand why "two arguments" means "includes payload", read
 * https://github.com/wasp-lang/wasp/issues/2050
 *
 * @template Input The type of the payload the operation expects.
 * @param args The arguments array for an authenticated operation.
 * @returns true if the arguments array includes a payload, false otherwise.
 */
function includesPayload<Input>(
  args: [AuthenticatedOperationContext] | [Input, AuthenticatedOperationContext]
): args is [Input, AuthenticatedOperationContext] {
  return args.length === 2
}

/**
 * Constructs the type of the arguments array for an authenticated operation
 * based on the type of its definition.
 */
type AuthenticatedOperationArgsFor<Op extends GenericAuthenticatedOperationDefinition> =
  Parameters<AuthenticatedOperationFor<Op>>

// NOTE: There's some duplication in the below types.
// Read the discussion here to understand why before attempting to remove it:
// https://github.com/wasp-lang/wasp/pull/2170#discussion_r1671285049
/**
 * Constructs the type for an authenticated operation's server-side API.
 *
 * @template Input The type of the payload the operation expects (must be
 * `void` if the operation doesn't expect a payload).
 * @template Output The type of the operation's return value.
 */
type AuthenticatedOperation<Input, Output> =
  IfAny<
    Input,
    (args: any, context: { user: AuthUser }) => Promise<Output>,
    AuthenticatedOperationWithNonAnyInput<Input, Output>
  >

// Read this to understand the type: https://github.com/wasp-lang/wasp/pull/1090#discussion_r1159732471
type AuthenticatedOperationWithNonAnyInput<Input, Output> =
  [Input] extends [never]
  ? (args: unknown, context: { user: AuthUser }) => Promise<Output>
  : [Input] extends [void]
  ? (context: { user: AuthUser }) => Promise<Output>
  : (args: Input, context: { user: AuthUser }) => Promise<Output>

/**
 * The principal type for an authenticated operation's definition (i.e., all
 * authenticated operation definition types are a subtype of this type).
 *
 */
type GenericAuthenticatedOperationDefinition = AuthenticatedOperationDefinition<
  // NOTE(filip): Not quite sure I understand what's going on with Variance here.
  _Entity[],
  never,
  Payload
>

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
