{{={= =}=}}
import { prisma } from 'wasp/server'
import { Expand, _Awaited, _ReturnType } from '../../../universal/types'

{=# isAuthEnabled =}
import { type AuthUser } from 'wasp/auth'
{=/ isAuthEnabled =}
import {
  AuthenticatedAction as AuthenticatedActionDefinition,
  Payload,
  EntityMap,
  Action as UnauthenticatedActionDefinition,
  _Entity,
  _Task,
} from '../../_types'
{=# operations =}
{=& jsFn.importStatement =}
{=/ operations =}

{=# operations =}
{=# usesAuth =}
export const {= operationName =}: AuthenticatedActionFor<typeof {= jsFn.importIdentifier =}>  = createAuthenticatedAction(
{=/ usesAuth =}
{=^ usesAuth =}
export const {= operationName =}: UnauthenticatedActionFor<typeof {= jsFn.importIdentifier =}>  = createUnauthenticatedAction(
{=/ usesAuth =}
    {= jsFn.importIdentifier =},
    {
      {=# entities =}
      {= name =}: prisma.{= prismaIdentifier =},
      {=/ entities =}
    } as Expand<EntityMap<_Entity[]>>,
)

{=/ operations =}
function createUnauthenticatedAction<
  Entities extends _Entity[],
  Input extends Payload,
  Output extends Payload
>(
  userAction: UnauthenticatedActionDefinition<Entities, Input, Output>,
  entities: Expand<EntityMap<Entities>>
): UnauthenticatedAction<Input, Output> {
  async function action(...args: UnauthenticatedOperationArgs<Input>) {
    // Maximum number of arguments is 1 - the payload
    if (args.length < 1) {
      // No payload sent -> 'userQuery' is void
      return userAction(undefined, {
        entities,
      })
    } else {
      // Payload sent -> forward it to 'userQuery'
      const [payload] = args
      return userAction(payload, {
        entities,
      })
    }
  }
  return action as UnauthenticatedAction<Input, Output>
}

function createAuthenticatedAction<
  Entities extends _Entity[],
  Input extends Payload,
  Output extends Payload
>(
  userAction: AuthenticatedActionDefinition<Entities, Input, Output>,
  entities: Expand<EntityMap<Entities>>
): AuthenticatedAction<Input, Output> {
  async function action(...args: AuthenticatedOperationArgs<Input>) {
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
    } else if (containsPayload(args)) {
      // Two arguments sent -> the first argument is the payload, the second is the context.
      const [payload, { user }] = args
      return userAction(payload, {
        user,
        entities,
      })
    } else {
      // One argument sent -> the first and only argument is the user.
      const [{ user }] = args
      return userAction(undefined, {
        user,
        entities,
      })
    }
  }
  return action as AuthenticatedAction<Input, Output>
}

function containsPayload<Input>(
  args: AuthenticatedOperationArgs<Input>
): args is [Input, OperationContext] {
  return args.length === 2
}

type UnauthenticatedOperationArgs<Input> = [Input] | []

type AuthenticatedOperationArgs<Input> =
  | [OperationContext]
  | [Input, OperationContext]

type OperationContext = { user: AuthUser }

type UnauthenticatedAction<Input, Output> = UnauthenticatedOperation<
  Input,
  Output
>

type AuthenticatedAction<Input, Output> = AuthenticatedOperation<Input, Output>

// type AuthenticatedActionFor<
//   AuthQ extends AuthenticatedActionDefinition<any, Payload, Payload>
// > = AuthQ extends AuthenticatedActionDefinition<any, infer Input, infer Output>
//   ? AuthenticatedAction<Input, Output>
//   : never

type GenericDefinition = (args: never, context: any) => unknown

type UnauthenticatedActionFor<OperationDefinition extends GenericDefinition> =
  Parameters<OperationDefinition> extends []
    ? UnauthenticatedOperation<void, _Awaited<_ReturnType<OperationDefinition>>>
    : UnauthenticatedOperation<
        Parameters<OperationDefinition>[0],
        _Awaited<_ReturnType<OperationDefinition>>
      >

type AuthenticatedActionFor<OperationDefinition extends GenericDefinition> =
  Parameters<OperationDefinition> extends []
    ? AuthenticatedOperation<void, _Awaited<_ReturnType<OperationDefinition>>>
    : AuthenticatedOperation<
        Parameters<OperationDefinition>[0],
        _Awaited<_ReturnType<OperationDefinition>>
      >

type UnauthenticatedOperation<Input, Output> = Operation<Input, Output, false>

type AuthenticatedOperation<Input, Output> = Operation<Input, Output, true>

type Operation<Input, Output, IsAuthenticated extends boolean> = [
  Input
] extends [void]
  ? OperationWithoutPayload<Output, IsAuthenticated>
  : OperationWithPayload<Input, Output, IsAuthenticated>

type OperationWithPayload<
  Input,
  Output,
  auth extends boolean
> = auth extends true
  ? (args: Input, context: { user: AuthUser }) => Promise<Output>
  : (args: Input) => Promise<Output>

type OperationWithoutPayload<
  Output,
  IsAuthenticated extends boolean
> = IsAuthenticated extends true
  ? (context: { user: AuthUser }) => Promise<Output>
  : () => Promise<Output>
