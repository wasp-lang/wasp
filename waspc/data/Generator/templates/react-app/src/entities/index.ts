{{={= =}=}}
import {
  {=# entities =}
  {= name =},
  {=/ entities =}
} from '@prisma/client'
  
export type {
  {=# entities =}
  {= name =},
  {=/ entities =}
  {=# isAuthEnabled =}
  {= authEntityName =},
  {= authIdentityEntityName =},
  {=/ isAuthEnabled =}
} from '@prisma/client'

export type Entity = 
  {=# entities =}
  | {= name =}
  {=/ entities =}
  | never
