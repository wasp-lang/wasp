{{={= =}=}}
// TODO: import auth entity and use it for "user"
{=# isAuthEnabled =}
import { type {= userEntityUpper =} } from '../entities/index.js'
{=/ isAuthEnabled =}
// TODO: make user optional if isPublic
import { type Prisma } from '@prisma/client'
export function createCrudOverrides(overrides: Overrides): Overrides {
  return overrides
}

type CreateArgs = Prisma.{= crud.entityUpper =}CreateArgs
type UpdateArgs = Prisma.{= crud.entityUpper =}UpdateArgs
type DeleteArgs = Prisma.{= crud.entityUpper =}DeleteArgs
type GetArgs = Prisma.{= crud.entityUpper =}FindUniqueArgs
type GetAllArgs = Prisma.{= crud.entityUpper =}FindManyArgs

{=# isAuthEnabled =}
type UserEntity = {= userEntityUpper =}
{=/ isAuthEnabled =}
{=^ isAuthEnabled =}
type UserEntity = any
{=/ isAuthEnabled =}

type Overrides = {
    {=# crud.operations.Get =}
    {=# isPublic =}
    Get?: (where: any, user?: UserEntity) => GetArgs
    {=/ isPublic =}
    {=^ isPublic =}
    Get?: (where: any, user: UserEntity) => GetArgs
    {=/ isPublic =}
    {=/ crud.operations.Get =}
    {=# crud.operations.GetAll =}
    {=# isPublic =}
    GetAll?: (where: any, user?: UserEntity) => GetAllArgs
    {=/ isPublic =}
    {=^ isPublic =}
    GetAll?: (where: any, user: UserEntity) => GetAllArgs
    {=/ isPublic =}
    {=/ crud.operations.GetAll =}
    {=# crud.operations.Create =}
    {=# isPublic =}
    Create?: (data: any, user?: UserEntity) => CreateArgs
    {=/ isPublic =}
    {=^ isPublic =}
    Create?: (data: any, user: UserEntity) => CreateArgs
    {=/ isPublic =}
    {=/ crud.operations.Create =}
    {=# crud.operations.Update =}
    {=# isPublic =}
    Update?: (data: any, user?: UserEntity) => UpdateArgs
    {=/ isPublic =}
    {=^ isPublic =}
    Update?: (data: any, user: UserEntity) => UpdateArgs
    {=/ isPublic =}
    {=/ crud.operations.Update =}
    {=# crud.operations.Delete =}
    {=# isPublic =}
    Delete?: (where: any, user?: UserEntity) => DeleteArgs
    {=/ isPublic =}
    {=^ isPublic =}
    Delete?: (where: any, user: UserEntity) => DeleteArgs
    {=/ isPublic =}
    {=/ crud.operations.Delete =}
}