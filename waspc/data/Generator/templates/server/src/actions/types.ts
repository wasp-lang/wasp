{{={= =}=}}
{=! TODO: This template is exactly the same at the moment as one for query
          types, consider whether it makes sense to address this in the future. =}
import {
  {=# allEntities =}
  {= . =},
  {=/ allEntities =}
} from '../entities'

import {
  {=# shouldImportNonAuthenticatedOperation =}
  Action,
  {=/ shouldImportNonAuthenticatedOperation =}
  {=# shouldImportAuthenticatedOperation =}
  AuthenticatedAction,
  {=/ shouldImportAuthenticatedOperation =}
} from '../types'

{=# operations =}
export type {= typeName =}<Input = never, Output = unknown> = 
  {=# usesAuth =}
  AuthenticatedAction<
  {=/ usesAuth =}
  {=^ usesAuth =}
  Action<
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
