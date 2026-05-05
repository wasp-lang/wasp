{{={= =}=}}
{=# areThereAnyEntitiesDefined =}
import { PrismaClient as InternalPrismaClient } from '@prisma/client'
import type { FromRegister } from 'wasp/types'

type UserPrismaSetupFn = FromRegister<'prismaSetupFn', () => InternalPrismaClient>;

{=# prismaSetupFn.isDefined =}
{=& prismaSetupFn.importStatement =}
const dbClient: PrismaClient =  {= prismaSetupFn.importIdentifier =}();
{=/ prismaSetupFn.isDefined =}
{=^ prismaSetupFn.isDefined =}
const dbClient: PrismaClient = new InternalPrismaClient();
{=/ prismaSetupFn.isDefined =}
{=/ areThereAnyEntitiesDefined =}
{=^ areThereAnyEntitiesDefined =}
// * Prisma will not generate a PrismaClient if there no
//   entities in the schema. Trying to init the PrismaClient
//   will throw an error.
// * To avoid throwing an error, we return null if there are no
//   entities in the schema.
const dbClient: null = null;
{=/ areThereAnyEntitiesDefined =}

// PUBLIC API
export type PrismaClient = ReturnType<UserPrismaSetupFn>;

// PUBLIC API
export default dbClient;
