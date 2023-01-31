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

type Entity = 
  {=# entities =}
  | {= name =}
  {=/ entities =}
  | never

export type EntityName = 
  {=# entities =}
  | "{= name =}"
  {=/ entities =}
  | never

{=# entities =}
export type {= internalTypeName =} = WithName<{= name =}, "{= name =}">
{=/ entities =}

export type _Entity = 
  {=# entities =}
  | {= internalTypeName =}
  {=/ entities =}
  | never

type WithName<E extends Entity, Name extends EntityName> = 
  E & { _waspEntityName: Name }
