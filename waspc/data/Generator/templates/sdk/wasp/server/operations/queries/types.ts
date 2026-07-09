{{={= =}=}}
{=! TODO: This template is exactly the same at the moment as one for action
          types, consider whether it makes sense to address this in the future. =}

import {
  {=# allEntities =}
  type {= internalTypeName =},
  {=/ allEntities =}
  {=# shouldImportNonAuthenticatedOperation =}
  type UnauthenticatedQueryDefinition,
  {=/ shouldImportNonAuthenticatedOperation =}
  {=# shouldImportAuthenticatedOperation =}
  type AuthenticatedQueryDefinition,
  {=/ shouldImportAuthenticatedOperation =}
  type Payload,
} from '../../_types/index.js'

// PUBLIC API
// Generic query type for code that can't reference generated per-operation
// types, e.g. Wasp modules. Less precise than the generated types: it knows
// nothing about entities or auth, so the implementor describes the context.
export type Query<Args = unknown, Result = unknown, Context = unknown> = (
  args: Args,
  context: Context,
) => Result | Promise<Result>

{=# operations =}
// PUBLIC API
export type {= typeName =}<Input extends Payload = never, Output extends Payload = Payload> =
  {=# usesAuth =}
  AuthenticatedQueryDefinition<
  {=/ usesAuth =}
  {=^ usesAuth =}
  UnauthenticatedQueryDefinition<
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
