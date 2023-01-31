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

// Wasp internally uses the following types for typing entity maps in
// operation contexts.
//
// We must explicitly tag all entities with their name to avoid issues with
// structural typing. See https://github.com/wasp-lang/wasp/pull/982 for details.

{=# entities =}
export type {= internalTypeName =} = WithName<{= name =}, "{= name =}">
{=/ entities =}

export type _Entity = 
  {=# entities =}
  | {= internalTypeName =}
  {=/ entities =}
  | never

type WithName<E extends Entity, Name extends EntityName> = 
  E & { _entityName: Name }
