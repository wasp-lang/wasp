{{={= =}=}}
import type {
  {=# allEntities =}
  {= internalTypeName =},
  {=/ allEntities =}
  TaskDefinition,
} from '../_types'

{=# tasks =}
// PUBLIC API
export type {= typeName =} = TaskDefinition<[
  {=# entities =}
  {= internalTypeName =},
  {=/ entities =}
]>

{=/ tasks =}
