{{={= =}=}}
import prisma from '../dbClient.js'
import { 
    WaspEntity,
    {=# entities =}
    {= name =},
    {=/ entities =}
 } from '../entities'

export type Query<Entities extends WaspEntity[], Result = unknown> = Operation<Entities, Result>

export type Action<Entities extends WaspEntity[], Result = unknown> = Operation<Entities, Result>

{=# isAuthEnabled =}
export type AuthenticatedQuery<Entities extends WaspEntity[], Result = unknown> = 
  AuthenticatedOperation<Entities, Result>

export type AuthenticatedAction<Entities extends WaspEntity[], Result = unknown> = 
  AuthenticatedOperation<Entities, Result>

type AuthenticatedOperation<Entities extends WaspEntity[], Result = unknown> = (
  args: any,
  context: {
      user: {= userViewName =},
      entities: EntityMap<Entities>,
  },
) => Promise<Result>

type {= userViewName =} = Omit<{= userEntityName =}, 'password'>
{=/ isAuthEnabled =}

type Operation<Entities extends WaspEntity[], Result = unknown> = (
  args: any,
  context: {
      entities: EntityMap<Entities>,
  },
) => Promise<Result>

type DelegateFor<EntityName extends string> =
  {=# entities =}
  EntityName extends "{= name =}" ? typeof prisma.{= prismaIdentifier =} :
  {=/ entities =}
  never

type NameOf<Entity extends WaspEntity> =
  {=# entities =}
  Entity extends {= name =} ? "{= name =}" :
  {=/ entities =}
  never

type EntityMap<Entities extends WaspEntity[]> = {
  [EntityName in NameOf<Entities[number]>]: DelegateFor<EntityName>
}
