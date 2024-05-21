{{={= =}=}}
import { _Awaited, _ReturnType } from '../../universal/types'

{=# isAuthEnabled =}
import { type AuthUser } from 'wasp/auth'
{=/ isAuthEnabled =}
import {
  _Entity,
  AuthenticatedOperationDefinition,
  Payload,
} from '../_types'

// PRIVATE API (used in SDK)
export type UnauthenticatedOperationFor<
  OperationDefinition extends GenericOperationDefinition
> = Parameters<OperationDefinition> extends []
  ? UnauthenticatedOperation<void, _Awaited<_ReturnType<OperationDefinition>>>
  : UnauthenticatedOperation<
      Parameters<OperationDefinition>[0],
      _Awaited<_ReturnType<OperationDefinition>>
    >

// PRIVATE API (used in SDK)
export function createUnauthenticatedOperation<Op extends GenericOperationDefinition>(
  userOperation: Op,
  entities: EntityMapFor<Op>
): UnauthenticatedOperationFor<Op> {
  async function operation(payload: Parameters<Op>[0]) {
    return userOperation(payload, {
      entities,
    })
  }
  // This cast is necessary because - When the Input is void, we want to present
  // the function as not accepting a payload (which isn't consistent with how
  // it's defined).
  return operation as UnauthenticatedOperationFor<Op>
}

{=# isAuthEnabled =}
// PRIVATE API (used in SDK)
export type AuthenticatedOperationFor<
  OperationDefinition extends GenericOperationDefinition
> = Parameters<OperationDefinition> extends []
  ? AuthenticatedOperation<void, _Awaited<_ReturnType<OperationDefinition>>>
  : AuthenticatedOperation<
      Parameters<OperationDefinition>[0],
      _Awaited<_ReturnType<OperationDefinition>>
    >

// PRIVATE API (used in SDK)
export function createAuthenticatedOperation<
  Op extends GenericOperationDefinition
>(
  userOperation: Op,
  entities: EntityMapFor<Op>
): AuthenticatedOperationFor<Op> {
  async function operation(...args: AuthenticatedOperationArgsFor<Op>) {
      /*
    An authenticated operation expects either a single argument or two arguments,
    depending on whether it was defined to accept a payload.
    The possible mutually-exclusive options for its type signatures are
    therefore:
     - action(payload, { user })
     - action({ user })

    Since neither the Generator nor the runtime know the function's true type
    signature (i.e., its true number of arguments), the runtime must "guess"
    which of the two possible signatures is right (i.e., whether
    the operation expects a payload or not) and act accordingly.
    
    When the user's using TypeScript, TypeScript takes care of enforcing that the
    user sends the correct number of arguments to this function, while the
    function's runtime decides on the correct implementation based on the number
    of arguments it received. As long as the user's using TypeScript,
    everything's OK 

    What happens when the user isn't using TypeScript or decides to bypass it?
    In this case, there are possible discrepancies between the function's true
    type and the type runtime decides on.

    Let's see what happens in all scenarios:

      - If the operation's meant to accept a payload, it should be called as:

        action(payload, { user })

      This is also the type advertised by TypeScript.
      Here's what happens when:
        - User calls it without arguments, `()` - All good, an appropriate error is
        thrown.
        - User calls it with a single argument, `action(arg1)` - Not good, the
        first arg is interpreted as the user, the action is called with
        `(undefined, { user: arg1 })`, while the correct call would be `(arg1, {
        user: undefined })`
        - User calls it with two arguments, `action(arg1, arg2)` - All good, `arg1`
        is treated as the payload, `arg2` is treated as the context.

      - If the operation isn't meant to accept a payload, it should be called as

        action({ user })

      This is also the type advertised by TypeScript.
      Here's what happens when:
        - User calls it with zero arguments, `action()` -> All good, an error is
        thrown.
        - User calls it with a single argument, `(arg1)` - All good, `arg1` is
        treated as the context
        - User calls it with two arguments, `action(arg1, arg2)` - Not good, `arg1`
        is treated as the payload, `arg2` is treated as the context.  The action
        is called with (arg1, { user: arg2 }), while the correct call would be
        `(undefined, { user: arg1 })`
    */
    if (args.length < 1) {
      // No arguments sent -> no user and no payload specified -> there's no way this was called correctly.
      throw new Error('Invalid number of arguments')
    } else if (includesPayload(args)) {
      // Two arguments sent -> the first argument is the payload, the second is the context.
      const [payload, { user }] = args
      return userOperation(payload, {
        user,
        entities,
      })
    } else {
      // One argument sent -> the first and only argument is the user.
      const [{ user }] = args
      return userOperation(undefined as Parameters<Op>[0], {
        user,
        entities,
      })
    }
  }

  return operation as AuthenticatedOperationFor<Op>
}

function includesPayload<Input>(
  args: [AuthenticatedOperationContext] | [Input, AuthenticatedOperationContext]
): args is [Input, AuthenticatedOperationContext] {
  return args.length === 2
}

type AuthenticatedOperationArgsFor<Op extends GenericOperationDefinition> = Parameters<
  AuthenticatedOperationFor<Op>
>

type AuthenticatedOperationContext = { user: AuthUser }

type AuthenticatedOperation<Input, Output> = Operation<Input, Output, true>

{=/ isAuthEnabled =}
type UnauthenticatedOperation<Input, Output> = Operation<Input, Output, false>

// todo(filip): Should i define this independently of AuthenticatedOperationDefinition?
// Using AuthenticatedOperationDefinition because it's  
type GenericOperationDefinition = AuthenticatedOperationDefinition<
  // todo(filip): not quite sure I understand what's going on with Variance here
  _Entity[],
  never,
  Payload
>

type EntityMapFor<Op extends GenericOperationDefinition> = Parameters<Op>[1]["entities"]

type Operation<Input, Output, IsAuthenticated extends boolean> = [
  Input
] extends [void]
  ? OperationWithoutPayload<Output, IsAuthenticated>
  : OperationWithPayload<Input, Output, IsAuthenticated>

type OperationWithPayload<
  Input,
  Output,
  IsAuthenticated extends boolean
> = IsAuthenticated extends true
  ? (args: Input, context: { user: AuthUser }) => Promise<Output>
  : (args: Input) => Promise<Output>

type OperationWithoutPayload<
  Output,
  IsAuthenticated extends boolean
> = IsAuthenticated extends true
  ? (context: { user: AuthUser }) => Promise<Output>
  : () => Promise<Output>
