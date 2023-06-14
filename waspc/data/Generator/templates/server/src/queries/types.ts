{{={= =}=}}
{=! TODO: This template is exactly the same at the moment as one for action
          types, consider whether it makes sense to address this in the future. =}

import {
  {=# allEntities =}
  type {= internalTypeName =},
  {=/ allEntities =}
  {=# shouldImportNonAuthenticatedOperation =}
  type Query,
  {=/ shouldImportNonAuthenticatedOperation =}
  {=# shouldImportAuthenticatedOperation =}
  type AuthenticatedQuery,
  {=/ shouldImportAuthenticatedOperation =}
  type Payload,
} from '../_types'

{=# operations =}
export type {= typeName =}<Input extends Payload = never, Output extends Payload = Payload> = 
  {=# usesAuth =}
  AuthenticatedQuery<
  {=/ usesAuth =}
  {=^ usesAuth =}
  Query<
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
