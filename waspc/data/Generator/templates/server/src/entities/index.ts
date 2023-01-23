{{={= =}=}}
import {
  {=# entities =}
  type {= name =},
  {=/ entities =}
} from "@prisma/client"
  
export {
  {=# entities =}
  type {= name =},
  {=/ entities =}
} from "@prisma/client"

export type WaspEntity = 
  {=# entities =}
  | {= name =}
  {=/ entities =}
  | never
