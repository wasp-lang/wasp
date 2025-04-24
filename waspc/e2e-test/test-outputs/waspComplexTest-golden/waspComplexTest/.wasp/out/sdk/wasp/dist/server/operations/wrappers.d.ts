import { IfAny, _Awaited, _ReturnType, _Parameters } from '../../universal/types';
import { type AuthUser } from 'wasp/auth';
import { _Entity, AuthenticatedOperationDefinition, UnauthenticatedOperationDefinition, Payload } from '../_types';
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
 * Constructs the authenticated operation's server-side API type from its
 * definition.
 *
 * @template OperationDefinition The type of the authenticated operation's
 * definition.
 */
export type AuthenticatedOperationFor<OperationDefinition extends GenericAuthenticatedOperationDefinition> = Parameters<OperationDefinition> extends [] ? AuthenticatedOperation<void, _Awaited<_ReturnType<OperationDefinition>>> : AuthenticatedOperation<Parameters<OperationDefinition>[0], _Awaited<_ReturnType<OperationDefinition>>>;
/**
 * The type of the context users must pass when calling an authenticated
 * operation's server-side API.
 */
export type AuthenticatedOperationContext = {
    user: AuthUser;
};
/**
 * Creates the server-side API for an authenticated operation.
 *
 * @template OperationDefinition The type of the authenticated operation's definition.
 * @param userOperation The authenticated operation's definition.
 * @param entities The authenticated operation's entity map .
 * @returns The server-side API for the provided authenticated operation.
 */
export declare function createAuthenticatedOperation<OperationDefinition extends GenericAuthenticatedOperationDefinition>(userOperation: OperationDefinition, entities: EntityMapFor<OperationDefinition>): AuthenticatedOperationFor<OperationDefinition>;
/**
 * Constructs the type for an authenticated operation's server-side API.
 *
 * @template Input The type of the payload the operation expects (must be
 * `void` if the operation doesn't expect a payload).
 * @template Output The type of the operation's return value.
 */
type AuthenticatedOperation<Input, Output> = IfAny<Input, (args: any, context: {
    user: AuthUser;
}) => Promise<Output>, AuthenticatedOperationWithNonAnyInput<Input, Output>>;
type AuthenticatedOperationWithNonAnyInput<Input, Output> = [
    Input
] extends [never] ? (args: unknown, context: {
    user: AuthUser;
}) => Promise<Output> : [Input] extends [void] ? (context: {
    user: AuthUser;
}) => Promise<Output> : (args: Input, context: {
    user: AuthUser;
}) => Promise<Output>;
/**
 * The principal type for an authenticated operation's definition (i.e., all
 * authenticated operation definition types are a subtype of this type).
 *
 */
type GenericAuthenticatedOperationDefinition = AuthenticatedOperationDefinition<_Entity[], never, Payload>;
/**
 * Constructs the type for an unauthenticated operation's server-side API.
 *
 * @template Input The type of the payload the operation expects (must be
 * `void` if the operation doesn't expect a payload).
 * @template Output The type of the operation's return value.
 */
type UnauthenticatedOperation<Input, Output> = IfAny<Input, (args?: any) => Promise<Output>, UnauthenticatedOperationWithNonAnyInput<Input, Output>>;
type UnauthenticatedOperationWithNonAnyInput<Input, Output> = [
    Input
] extends [never] ? (args?: unknown) => Promise<Output> : [Input] extends [void] ? () => Promise<Output> : (args: Input) => Promise<Output>;
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
type EntityMapFor<OperationDefinition extends GenericUnauthenticatedOperationDefinition> = _Parameters<OperationDefinition>[1]["entities"];
export {};
//# sourceMappingURL=wrappers.d.ts.map