{{={= =}=}}
import { Request, Response } from 'express'
import { ParamsDictionary as ExpressParams, Query as ExpressQuery } from 'express-serve-static-core'
import prisma from "../dbClient.js"
{=# isAuthEnabled =}
import { type {= userEntityName =} } from "../entities"
{=/ isAuthEnabled =}
import { type _Entity } from "./taggedEntities"

export * from "./taggedEntities"

export type Query<Entities extends _Entity[], Input, Output> = Operation<Entities, Input, Output>

export type Action<Entities extends _Entity[], Input, Output> = Operation<Entities, Input, Output>

{=# isAuthEnabled =}
export type AuthenticatedQuery<Entities extends _Entity[], Input, Output> = 
  AuthenticatedOperation<Entities, Input, Output>

export type AuthenticatedAction<Entities extends _Entity[], Input, Output> = 
  AuthenticatedOperation<Entities, Input, Output>

type AuthenticatedOperation<Entities extends _Entity[], Input, Output> = (
  args: Input,
  context: Expand<OperationContext<Entities> & { 
  // TODO: This type must match the logic in core/auth.js (if we remove the
  // password field from the object there, we must do the same here). Ideally,
  // these two things would live in the same place:
  // https://github.com/wasp-lang/wasp/issues/965
    {= userFieldName =}: {= userEntityName =}WithoutPassword 
  }>,
) => Promise<Output>

export type {= userEntityName =}WithoutPassword = Omit<{= userEntityName =}, 'password'>

export type AuthenticatedApi<
  Entities extends _Entity[],
  P extends ExpressParams = ExpressParams,
  ResBody = any,
  ReqBody = any,
  ReqQuery extends ExpressQuery = ExpressQuery,
  Locals extends Record<string, any> = Record<string, any>
> = (
  req: Request<P, ResBody, ReqBody, ReqQuery, Locals>,
  res: Response<ResBody, Locals>,
  context: Expand<ApiContext<Entities> & { 
      {= userFieldName =}: {= userEntityName =}WithoutPassword
    }>,
) => any

{=/ isAuthEnabled =}
type Operation<Entities extends _Entity[], Input, Output> = (
  args: Input,
  context: Expand<OperationContext<Entities>>,
) => Promise<Output>

type OperationContext<Entities extends _Entity[]> = {
  entities: Expand<EntityMap<Entities>>
}

export type Api<
  Entities extends _Entity[],
  P extends ExpressParams = ExpressParams,
  ResBody = any,
  ReqBody = any,
  ReqQuery extends ExpressQuery = ExpressQuery,
  Locals extends Record<string, any> = Record<string, any>
> = (
  req: Request<P, ResBody, ReqBody, ReqQuery, Locals>,
  res: Response<ResBody, Locals>,
  context: Expand<ApiContext<Entities>>,
) => any

type ApiContext<Entities extends _Entity[]> = {
  entities: Expand<EntityMap<Entities>>
}

type EntityMap<Entities extends _Entity[]> = {
  [EntityName in Entities[number]["_entityName"]]: PrismaDelegate[EntityName]
}

type PrismaDelegate = {
  {=# entities =}
  "{= name =}": typeof prisma.{= prismaIdentifier =},
  {=/ entities =}
}

// This is a helper type used exclusively for DX purposes. It's a No-op for the
// compiler, but expands the type's representatoin in IDEs (i.e., inlines all
// type constructors) to make it more readable for the user.
//
// Check this SO answer for details: https://stackoverflow.com/a/57683652
type Expand<T extends object> = T extends infer O ? { [K in keyof O]: O[K] } : never
