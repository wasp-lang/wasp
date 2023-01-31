{{={= =}=}}
import prisma from "../dbClient.js"
import { 
  type _Entity,
  type {= userEntityName =}
 } from "../entities"

export type Query<Entities extends _Entity[] = [], Result = unknown> = Operation<Entities, Result>

export type Action<Entities extends _Entity[] = [], Result = unknown> = Operation<Entities, Result>

{=# isAuthEnabled =}
export type AuthenticatedQuery<Entities extends _Entity[] = [], Result = unknown> = 
  AuthenticatedOperation<Entities, Result>

export type AuthenticatedAction<Entities extends _Entity[] = [], Result = unknown> = 
  AuthenticatedOperation<Entities, Result>

type AuthenticatedOperation<Entities extends _Entity[], Result> = (
  args: any,
  context: {
    user: {= userViewName =},
    entities: Expand<EntityMap<Entities>>,
  },
) => Promise<Result>

// TODO: This type must match the logic in core/auth.js (if we remove the
// password field from the object there, we must do the same here). Ideally,
// these two things would live in the same place:
// https://github.com/wasp-lang/wasp/issues/965
type {= userViewName =} = Omit<{= userEntityName =}, 'password'>
{=/ isAuthEnabled =}

type Operation<Entities extends _Entity[], Result> = (
  args: any,
  context: {
    entities: Expand<EntityMap<Entities>>,
  },
) => Promise<Result>

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
type Expand<T extends object> = T extends infer O ? { [K in keyof O]: O[K] } : never;
