{{={= =}=}}
import {
  {=# isAuthEnabled =}
  type {= userEntityUpper =},
  {=/ isAuthEnabled =}
} from '../entities/index.js'
// TODO(miho): replace with proper imports once we support type imports
import { PrismaArgs, type RouteInputs } from '../universal/crud/{= crud.name =}.js'
import { Expand, PartialBy } from '../universal/types.js'

export type { PrismaArgs, RouteInputs }

{=# isAuthEnabled =}
type UserEntity = {= userEntityUpper =}
{=/ isAuthEnabled =}
{=^ isAuthEnabled =}
type UserEntity = any
{=/ isAuthEnabled =}

namespace OverridesReturnValues {
  // We provide the default 'where' property for Get operation
  export type Get = Expand<PartialBy<PrismaArgs.Get, 'where'>>
  export type GetAll = PrismaArgs.GetAll
  // We provide the default 'data' property for Create operation
  export type Create = Expand<PartialBy<PrismaArgs.Create, 'data'>>
  // We provide the default 'where' and 'data' properties for Update operation
  export type Update = Expand<PartialBy<PrismaArgs.Update, 'where' | 'data'>>
  // We provide the default 'where' property for Delete operation
  export type Delete = Expand<PartialBy<PrismaArgs.Delete, 'where'>>
}

type Overrides = {
    {=# crud.operations.Get =}
    {=# isPublic =}
    Get?: (args: RouteInputs.Get, user?: UserEntity) => OverridesReturnValues.Get
    {=/ isPublic =}
    {=^ isPublic =}
    Get?: (args: RouteInputs.Get, user: UserEntity) => OverridesReturnValues.Get
    {=/ isPublic =}
    {=/ crud.operations.Get =}
    {=# crud.operations.GetAll =}
    {=# isPublic =}
    GetAll?: (args: RouteInputs.GetAll, user?: UserEntity) => OverridesReturnValues.GetAll
    {=/ isPublic =}
    {=^ isPublic =}
    GetAll?: (args: RouteInputs.GetAll, user: UserEntity) => OverridesReturnValues.GetAll
    {=/ isPublic =}
    {=/ crud.operations.GetAll =}
    {=# crud.operations.Create =}
    {=# isPublic =}
    Create?: (args: RouteInputs.Create, user?: UserEntity) => OverridesReturnValues.Create
    {=/ isPublic =}
    {=^ isPublic =}
    Create?: (args: RouteInputs.Create, user: UserEntity) => OverridesReturnValues.Create
    {=/ isPublic =}
    {=/ crud.operations.Create =}
    {=# crud.operations.Update =}
    {=# isPublic =}
    Update?: (args: RouteInputs.Update, user?: UserEntity) => OverridesReturnValues.Update
    {=/ isPublic =}
    {=^ isPublic =}
    Update?: (args: RouteInputs.Update, user: UserEntity) => OverridesReturnValues.Update
    {=/ isPublic =}
    {=/ crud.operations.Update =}
    {=# crud.operations.Delete =}
    {=# isPublic =}
    Delete?: (args: RouteInputs.Delete, user?: UserEntity) => OverridesReturnValues.Delete
    {=/ isPublic =}
    {=^ isPublic =}
    Delete?: (args: RouteInputs.Delete, user: UserEntity) => OverridesReturnValues.Delete
    {=/ isPublic =}
    {=/ crud.operations.Delete =}
}

export function createCrudOverrides(overrides: Overrides): Overrides {
  return overrides
}