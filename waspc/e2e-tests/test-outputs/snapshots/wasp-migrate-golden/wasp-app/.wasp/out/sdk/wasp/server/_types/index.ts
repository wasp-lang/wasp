import { type Expand } from '../../universal/types.js'
import { type Request, type Response } from 'express'
import {
  type ParamsDictionary as ExpressParams,
  type Query as ExpressQuery,
} from 'express-serve-static-core'
import { prisma } from '../index.js'
import {
  type ActionFn,
  type ApiFn,
  type OperationFn,
  type QueryFn,
} from '../types/base.js'
import { type _Entity } from './taggedEntities'
import { type Payload } from '../../core/serialization/index.js'

export * from "./taggedEntities"
export * from "../../core/serialization/index.js"

export type UnauthenticatedQueryDefinition<
  Entities extends _Entity[],
  Input extends Payload,
  Output extends Payload
> = QueryFn<Input, Output, Context<Entities>>

export type UnauthenticatedActionDefinition<
  Entities extends _Entity[],
  Input extends Payload,
  Output extends Payload
> = ActionFn<Input, Output, Context<Entities>>

export type UnauthenticatedOperationDefinition<
  Entities extends _Entity[],
  Input extends Payload,
  Output extends Payload
> = OperationFn<Input, Output, Context<Entities>>

export type Api<
  Entities extends _Entity[],
  Params extends ExpressParams,
  ResBody,
  ReqBody,
  ReqQuery extends ExpressQuery,
  Locals extends Record<string, any>
> = ApiFn<
  Request<Params, ResBody, ReqBody, ReqQuery, Locals>,
  Response<ResBody, Locals>,
  Context<Entities>
>

export type EntityMap<Entities extends _Entity[]> = {
  [EntityName in Entities[number]["_entityName"]]: PrismaDelegate[EntityName]
}

export type PrismaDelegate = {
  "Task": typeof prisma.task,
}

type Context<Entities extends _Entity[]> = Expand<{
  entities: Expand<EntityMap<Entities>>
}>

