{{={= =}=}}
import {
  {=# allEntities =}
  {= . =},
  {=/ allEntities =}
} from '../entities'

import {
  {=^ allUseAuth =}
  Query,
  {=/ allUseAuth =}
  {=# someUseAuth =}
  AuthenticatedQuery,
  {=/ someUseAuth =}
} from '../types'

{=# operations =}
export type {= typeName =}<Output = unknown> = 
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
    Output
  >

{=/ operations =}
