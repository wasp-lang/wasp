import { _Awaited, _ReturnType } from '../../universal/types';
import { _Entity, UnauthenticatedOperationDefinition, Payload } from '../_types';
/**
 * Constructs the unauthenticated operation's server-side API type from its
 * definition.
 *
 * @template OperationDefinition The type of the unauthenticated operation's
 * definition.
 */
export type UnauthenticatedOperationFor<OperationDefinition extends GenericUnauthenticatedOperationDefinition> = Parameters<OperationDefinition> extends [] ? UnauthenticatedOperation<void, _Awaited<_ReturnType<OperationDefinition>>> : UnauthenticatedOperation<Parameters<OperationDefinition>[0], _Awaited<_ReturnType<OperationDefinition>>>;
/**
 * Creates the server-side API for an unauthenticated operation.
 *
 * @template OperationDefinition The type of the unauthenticated operation's definition.
 * @param userOperation The unauthenticated operation's definition.
 * @param entities The unauthenticated operation's entity map .
 * @returns The server-side API for the provided unauthenticated operation.
 */
export declare function createUnauthenticatedOperation<OperationDefinition extends GenericUnauthenticatedOperationDefinition>(userOperation: OperationDefinition, entities: EntityMapFor<OperationDefinition>): UnauthenticatedOperationFor<OperationDefinition>;
/**
 * Constructs the type for an unauthenticated operation's server-side API.
 *
 * @template Input The type of the payload the operation expects (must be
 * `void` if the operation doesn't expect a payload).
 * @template Output The type of the operation's return value.
 */
type UnauthenticatedOperation<Input, Output> = [
    Input
] extends [never] ? (args: unknown) => Promise<Output> : [Input] extends [void] ? () => Promise<Output> : (args: Input) => Promise<Output>;
/**
 * The principal type for an unauthenticated operation's definition (i.e., all
 * unauthenticated operation definition types are a subtype of this type).
 *
 */
type GenericUnauthenticatedOperationDefinition = UnauthenticatedOperationDefinition<_Entity[], never, Payload>;
/**
 * Queries the entity map from the type of the operation's definition.
 *
 * @template OperationDefinition The type of the operation's definition.
 */
type EntityMapFor<OperationDefinition extends GenericUnauthenticatedOperationDefinition> = Parameters<OperationDefinition>[1]["entities"];
export {};
