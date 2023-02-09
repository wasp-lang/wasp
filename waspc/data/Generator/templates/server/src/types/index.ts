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
  context: Expand<OperationContext<Entities> & { 
  // TODO: This type must match the logic in core/auth.js (if we remove the
  // password field from the object there, we must do the same here). Ideally,
  // these two things would live in the same place:
  // https://github.com/wasp-lang/wasp/issues/965
    {= userFieldName =}: Omit<{= userEntityName =}, 'password'> 
  }>,
) => Promise<Result>
{=/ isAuthEnabled =}

type Operation<Entities extends _Entity[], Result> = (
  args: any,
  context: Expand<OperationContext<Entities>>,
) => Promise<Result>

type OperationContext<Entities extends _Entity[]> = {
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
type Expand<T extends object> = T extends infer O ? { [K in keyof O]: O[K] } : never;
