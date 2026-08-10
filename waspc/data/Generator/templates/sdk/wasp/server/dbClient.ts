{{={= =}=}}
{=# areThereAnyEntitiesDefined =}
import { PrismaClient as InternalPrismaClient } from '@prisma/client'
import type { FromRegister } from '../types/register'
{=# prismaSetupFn.isDefined =}
{=& prismaSetupFn.importStatement =}
{=/ prismaSetupFn.isDefined =}

// PUBLIC API
export type PrismaClient = ReturnType<RegisteredPrismaSetupFn>;

export type RegisteredPrismaSetupFn = FromRegister<'prismaSetupFn', () => InternalPrismaClient>;

{=# prismaSetupFn.isDefined =}
const dbClient: PrismaClient =  {= prismaSetupFn.importIdentifier =}();
{=/ prismaSetupFn.isDefined =}
{=^ prismaSetupFn.isDefined =}
const dbClient: PrismaClient = new InternalPrismaClient();
{=/ prismaSetupFn.isDefined =}
{=/ areThereAnyEntitiesDefined =}
{=^ areThereAnyEntitiesDefined =}
export type PrismaClient = null;

// * Prisma will not generate a PrismaClient if there no
//   entities in the schema. Trying to init the PrismaClient
//   will throw an error.
// * To avoid throwing an error, we return null if there are no
//   entities in the schema.
const dbClient: PrismaClient = null;
{=/ areThereAnyEntitiesDefined =}

// PUBLIC API
export default dbClient;
