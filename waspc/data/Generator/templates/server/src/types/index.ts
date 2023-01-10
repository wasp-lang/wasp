{{={= =}=}}
import prisma from '../dbClient.js'
import {
    {=# entities =}
    {= name =},
    {=/ entities =}
} from '@prisma/client'

export {
    {=# entities =}
    {= name =},
    {=/ entities =}
} from '@prisma/client'

{=# isAuthEnabled =}
export type {= userEntityName =}View = Omit<{= userEntityName =}, 'password'>
{=/ isAuthEnabled =}

type DelegateFor<EntityName> =
    {=# entities =}
    EntityName extends "{= name =}" ? typeof prisma.{= prismaIdentifier =} :
    {=/ entities =}
    never

type NameOf<Entity> =
    {=# entities =}
    Entity extends {= name =} ? "{= name =}" :
    {=/ entities =}
    never

type EntityMap<Entities extends unknown[]> = {
    [EntityName in NameOf<Entities[number]>]: DelegateFor<EntityName>
}

export type Query<Entities extends unknown[], R = unknown> = (
    args: any,
    context: {
        entities: EntityMap<Entities>,
    },
) => Promise<R>


{=# isAuthEnabled =}
export type AuthenticatedQuery<Entities extends unknown[], R = unknown> = (
    args: any,
    context: {
        user: {= userEntityName =}View,
        entities: EntityMap<Entities>,
    },
) => Promise<R>
{=/ isAuthEnabled =}
