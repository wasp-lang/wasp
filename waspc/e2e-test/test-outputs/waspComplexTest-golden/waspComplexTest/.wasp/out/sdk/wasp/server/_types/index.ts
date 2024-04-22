import { type Expand } from 'wasp/universal/types';
import { type Request, type Response } from 'express'
import { type ParamsDictionary as ExpressParams, type Query as ExpressQuery } from 'express-serve-static-core'
import { prisma } from 'wasp/server'
import {
  type AuthIdentity,
} from "wasp/entities"
import {
  type EmailProviderData,
  type UsernameProviderData,
  type OAuthProviderData,
} from 'wasp/auth/utils'
import { type AuthUser } from 'wasp/auth'
import { type _Entity } from "./taggedEntities"
import { type Payload } from "./serialization";

export * from "./taggedEntities"
export * from "./serialization"

export type Query<Entities extends _Entity[], Input extends Payload, Output extends Payload> = 
  Operation<Entities, Input, Output>

export type Action<Entities extends _Entity[], Input extends Payload, Output extends Payload> = 
  Operation<Entities, Input, Output>

export type AuthenticatedQuery<Entities extends _Entity[], Input extends Payload, Output extends Payload> = 
  AuthenticatedOperation<Entities, Input, Output>

export type AuthenticatedAction<Entities extends _Entity[], Input extends Payload, Output extends Payload> = 
  AuthenticatedOperation<Entities, Input, Output>

type AuthenticatedOperation<Entities extends _Entity[], Input extends Payload, Output extends Payload> = (
  args: Input,
  context: ContextWithUser<Entities>,
) => Output | Promise<Output>

export type AuthenticatedApi<
  Entities extends _Entity[],
  Params extends ExpressParams,
  ResBody,
  ReqBody,
  ReqQuery extends ExpressQuery,
  Locals extends Record<string, any>
> = (
  req: Request<Params, ResBody, ReqBody, ReqQuery, Locals>,
  res: Response<ResBody, Locals>,
  context: ContextWithUser<Entities>,
) => void

type Operation<Entities extends _Entity[], Input, Output> = (
  args: Input,
  context: Context<Entities>,
) => Output | Promise<Output>

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

type EntityMap<Entities extends _Entity[]> = {
  [EntityName in Entities[number]["_entityName"]]: PrismaDelegate[EntityName]
}

export type PrismaDelegate = {
  "User": typeof prisma.user,
  "SocialLogin": typeof prisma.socialLogin,
  "Task": typeof prisma.task,
}

type Context<Entities extends _Entity[]> = Expand<{
  entities: Expand<EntityMap<Entities>>
}>

type ContextWithUser<Entities extends _Entity[]> = Expand<Context<Entities> & { user?: AuthUser }>

export type { ProviderName } from 'wasp/auth/utils'
