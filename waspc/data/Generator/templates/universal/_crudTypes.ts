{{={= =}=}}
import { type Prisma, type {= crud.entityUpper =} } from '@prisma/client'
import { Expand } from '../types'

export type EntityType = {= crud.entityUpper =}

export namespace PrismaArgs {
    export type GetAll = Prisma.{= crud.entityUpper =}FindManyArgs
    export type Get = Prisma.{= crud.entityUpper =}FindUniqueArgs
    export type Create = Prisma.{= crud.entityUpper =}CreateArgs
    export type Update = Prisma.{= crud.entityUpper =}UpdateArgs
    export type Delete = Prisma.{= crud.entityUpper =}DeleteArgs
}

export namespace RouteInputs {
    type PrimaryFieldType = {= crud.entityUpper =}["{= crud.primaryFieldName =}"]
    type PrimaryFieldArgs = { {= crud.primaryFieldName =}: PrimaryFieldType }

    export type GetAll = {}
    export type Get = Expand<PrimaryFieldArgs>
    export type Create = Expand<Partial<Omit<EntityType, "{= crud.primaryFieldName =}">>>
    export type Update = Expand<Partial<Create> & PrimaryFieldArgs>
    export type Delete = Expand<PrimaryFieldArgs>
}
