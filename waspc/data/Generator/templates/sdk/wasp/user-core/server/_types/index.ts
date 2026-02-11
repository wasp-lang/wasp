{{={= =}=}}
import { type Expand } from '../../../core/universal/types.js'
import { type Request, type Response } from 'express'
import {
  type ParamsDictionary as ExpressParams,
  type Query as ExpressQuery,
} from 'express-serve-static-core'
import { prisma } from 'wasp/server'
{=# isAuthEnabled =}
import { type AuthUser } from 'wasp/auth'
{=/ isAuthEnabled =}
import { type _Entity } from '../../../core/server/_types/taggedEntities'
import { type Payload } from '../../../core/core/serialization/index.js'

export * from "../../../core/server/_types/taggedEntities"
export * from '../../../core/core/serialization/index.js'

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

{=# isAuthEnabled =}
export type AuthenticatedQueryDefinition<
  Entities extends _Entity[],
  Input extends Payload,
  Output extends Payload
> = AuthenticatedOperationDefinition<Entities, Input, Output>

export type AuthenticatedActionDefinition<
  Entities extends _Entity[],
  Input extends Payload,
  Output extends Payload
> = AuthenticatedOperationDefinition<Entities, Input, Output>

export type AuthenticatedOperationDefinition<
  Entities extends _Entity[],
  Input extends Payload,
  Output extends Payload
> = (
  args: Input,
  context: ContextWithUser<Entities>
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

{=/ isAuthEnabled =}
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
  {=# entities =}
  "{= name =}": typeof prisma.{= prismaIdentifier =},
  {=/ entities =}
}

type Context<Entities extends _Entity[]> = Expand<{
  entities: Expand<EntityMap<Entities>>
}>

{=# isAuthEnabled =}
type ContextWithUser<Entities extends _Entity[]> = Expand<
  Context<Entities> & { user?: AuthUser }
>

export type { ProviderName } from 'wasp/auth/utils'
{=/ isAuthEnabled =}
