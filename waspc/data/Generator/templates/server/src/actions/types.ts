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
} from '../_types'

{=# operations =}
export type {= typeName =}<Output = unknown> = 
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
    Output
  >

{=/ operations =}
