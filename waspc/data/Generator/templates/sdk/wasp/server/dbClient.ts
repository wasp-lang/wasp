{{={= =}=}}
{=# areThereAnyEntitiesDefined =}
{=# prismaSetupFn.isDefined =}
{=& prismaSetupFn.importStatement =}
const userPrismaSetupFn: typeof {= prismaSetupFn.importIdentifier =} = {= prismaSetupFn.importIdentifier =};
const dbClient = userPrismaSetupFn();
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
