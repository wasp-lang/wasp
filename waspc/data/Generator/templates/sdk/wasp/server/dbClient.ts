{{={= =}=}}
{=# areThereAnyEntitiesDefined =}
{=# prismaSetupFn.isDefined =}
// @ts-expect-error
{=& prismaSetupFn.importStatement =}
import type { PrismaClient as InternalPrismaClient } from '@prisma/client'
import type { FromRegistry } from 'wasp/types'

type UserPrismaSetupFn = FromRegistry<'prismaSetupFn', () => InternalPrismaClient>;
const dbClient: ReturnType<UserPrismaSetupFn> =  {= prismaSetupFn.importIdentifier =}();
{=/ prismaSetupFn.isDefined =}
{=^ prismaSetupFn.isDefined =}
import { PrismaClient as InternalPrismaClient } from '@prisma/client'
const dbClient = InternalPrismaClient();
{=/ prismaSetupFn.isDefined =}
{=/ areThereAnyEntitiesDefined =}
{=^ areThereAnyEntitiesDefined =}
// * Prisma will not generate a PrismaClient if there no
//   entities in the schema. Trying to init the PrismaClient
//   will throw an error.`
// * To avoid throwing an error, we return null if there are no
//   entities in the schema.
const dbClient = null;
{=/ areThereAnyEntitiesDefined =}

// PUBLIC API
export type PrismaClient = typeof dbClient;
// PUBLIC API
export default dbClient;
