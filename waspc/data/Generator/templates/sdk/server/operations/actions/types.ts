{{={= =}=}}
{=! TODO: This template is exactly the same at the moment as one for query
          types, consider whether it makes sense to address this in the future. =}
import {
  {=# allEntities =}
  type {= internalTypeName =},
  {=/ allEntities =}
  {=# shouldImportNonAuthenticatedOperation =}
  type Action,
  {=/ shouldImportNonAuthenticatedOperation =}
  {=# shouldImportAuthenticatedOperation =}
  type AuthenticatedAction,
  {=/ shouldImportAuthenticatedOperation =}
  type Payload,
} from 'wasp/server/_types'

{=# operations =}
// PUBLIC API
export type {= typeName =}<Input extends Payload = never, Output extends Payload = Payload> = 
  {=# usesAuth =}
  AuthenticatedAction<
  {=/ usesAuth =}
  {=^ usesAuth =}
  Action<
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