import { type Expand } from 'wasp/universal/types'
import { type Request, type Response } from 'express'
import {
  type ParamsDictionary as ExpressParams,
  type Query as ExpressQuery,
} from 'express-serve-static-core'
import { prisma } from 'wasp/server'
import { type _Entity } from './taggedEntities'
import { type Payload } from 'wasp/core/serialization'

export * from "./taggedEntities"
export * from "wasp/core/serialization"

export type UnauthenticatedQueryDefinition<
  Entities extends _Entity[],
  Input extends Payload,
  Output extends Payload
> = UnauthenticatedOperationDefinition<Entities, Input, Output>

export type UnauthenticatedActionDefinition<
  Entities extends _Entity[],
  Input extends Payload,
  Output extends Payload
> = UnauthenticatedOperationDefinition<Entities, Input, Output>

export type UnauthenticatedOperationDefinition<
  Entities extends _Entity[],
  Input extends Payload,
  Output extends Payload
> = (args: Input, context: Context<Entities>) => Output | Promise<Output>

export type Api<
  Entities extends _Entity[],
  Params extends ExpressParams,
  ResBody,
  ReqBody,
  ReqQuery extends ExpressQuery,
  Locals extends Record<string, any>
> = (
  req: Request<Params, ResBody, ReqBody, ReqQuery, Locals>,
  res: Response<ResBody, Locals>,
  context: Context<Entities>,
) => void

export type EntityMap<Entities extends _Entity[]> = {
  [EntityName in Entities[number]["_entityName"]]: PrismaDelegate[EntityName]
}

export type PrismaDelegate = {
  "Task": typeof prisma.task,
}

type Context<Entities extends _Entity[]> = Expand<{
  entities: Expand<EntityMap<Entities>>
}>

