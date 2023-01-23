{{={= =}=}}
import prisma from "../dbClient.js"
import { 
  type WaspEntity,
  {=# entities =}
  type {= name =},
  {=/ entities =}
 } from "../entities"

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

// TODO: This type must match the logic in core/auth.js (we remove the
// password field from the object there, so we need to do the same here).
// Ideally, these two things would live in the same place:
// https://github.com/wasp-lang/wasp/issues/965
type {= userViewName =} = Omit<{= userEntityName =}, 'password'>
{=/ isAuthEnabled =}

type Operation<Entities extends WaspEntity[], Result = unknown> = (
  args: any,
  context: {
      entities: EntityMap<Entities>,
  },
) => Promise<Result>

type PrismaDelegateFor<EntityName extends string> =
  {=# entities =}
  EntityName extends "{= name =}" ? typeof prisma.{= prismaIdentifier =} :
  {=/ entities =}
  never

type WaspNameFor<Entity extends WaspEntity> =
  {=# entities =}
  Entity extends {= name =} ? "{= name =}" :
  {=/ entities =}
  never

type EntityMap<Entities extends WaspEntity[]> = {
  [EntityName in WaspNameFor<Entities[number]>]: PrismaDelegateFor<EntityName>
}
