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
  {=# isAuthEnabled =}
  type {= authEntityName =},
  type {= authIdentityEntityName =},
  {=/ isAuthEnabled =}
} from "@prisma/client"

export type Entity = 
  {=# entities =}
  | {= name =}
  {=/ entities =}
  | never

export type EntityName = 
  {=# entities =}
  | "{= name =}"
  {=/ entities =}
  | never
