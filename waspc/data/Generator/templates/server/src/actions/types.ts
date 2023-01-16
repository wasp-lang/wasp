{{={= =}=}}
import {
  {=# allEntities =}
  {= . =},
  {=/ allEntities =}
} from '../entities'

import {
  {=^ allUseAuth =}
  Action,
  {=/ allUseAuth =}
  {=# someUseAuth =}
  AuthenticatedAction,
  {=/ someUseAuth =}
} from '../types'

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
      {=.=},
    {=/ entities =}
    ],
    Output
  >

{=/ operations =}
