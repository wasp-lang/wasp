{{={= =}=}}
{=! TODO: This template is exactly the same at the moment as one for query
          types, consider whether it makes sense to address this in the future. =}
import {
  {=# allEntities =}
  type {= internalTypeName =},
  {=/ allEntities =}
  {=# shouldImportNonAuthenticatedOperation =}
  type UnauthenticatedActionDefinition,
  {=/ shouldImportNonAuthenticatedOperation =}
  {=# shouldImportAuthenticatedOperation =}
  type AuthenticatedActionDefinition,
  {=/ shouldImportAuthenticatedOperation =}
  type Payload,
} from '../../_types/index.js'

// PUBLIC API
// Generic action type for code that can't reference generated per-operation
// types, e.g. Wasp modules. Less precise than the generated types: it knows
// nothing about entities or auth, so the implementor describes the context.
export type Action<Args = unknown, Result = unknown, Context = unknown> = (
  args: Args,
  context: Context,
) => Result | Promise<Result>

{=# operations =}
// PUBLIC API
export type {= typeName =}<Input extends Payload = never, Output extends Payload = Payload> =
  {=# usesAuth =}
  AuthenticatedActionDefinition<
  {=/ usesAuth =}
  {=^ usesAuth =}
  UnauthenticatedActionDefinition<
  {=/ usesAuth =}
    [
    {=# entities =}
      {= internalTypeName =},
    {=/ entities =}
    ],
    Input,
    Output
  >

{=/ operations =}
