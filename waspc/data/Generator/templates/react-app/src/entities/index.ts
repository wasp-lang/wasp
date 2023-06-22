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
} from '@prisma/client'

export type Entity = 
  {=# entities =}
  | {= name =}
  {=/ entities =}
  | never
