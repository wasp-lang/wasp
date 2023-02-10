{{={= =}=}}
{=! TODO: This template is exactly the same at the moment as one for action
          types, consider whether it makes sense to address this in the future. =}
import {
  {=# allEntities =}
  {= . =},
  {=/ allEntities =}
} from '../entities'

import {
  {=# shouldImportNonAuthenticatedOperation =}
  Query,
  {=/ shouldImportNonAuthenticatedOperation =}
  {=# shouldImportAuthenticatedOperation =}
  AuthenticatedQuery,
  {=/ shouldImportAuthenticatedOperation =}
} from '../types'

{=# operations =}
export type {= typeName =}<Input = never, Output = unknown> = 
  {=# usesAuth =}
  AuthenticatedQuery<
  {=/ usesAuth =}
  {=^ usesAuth =}
  Query<
  {=/ usesAuth =}
    [
    {=# entities =}
      {=.=},
    {=/ entities =}
    ],
    Input,
    Output
  >

{=/ operations =}
