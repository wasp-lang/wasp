{{={= =}=}}
{=# areThereAnyEntitiesDefined =}
import { PrismaClient as InternalPrismaClient } from '@prisma/client'
import type { FromRegistry } from 'wasp/types'

type UserPrismaSetupFn = FromRegistry<'prismaSetupFn', () => InternalPrismaClient>;
type PrismaClientResolved = ReturnType<UserPrismaSetupFn>;

{=# prismaSetupFn.isDefined =}
// @ts-expect-error
{=& prismaSetupFn.importStatement =}
const dbClient: PrismaClientResolved =  {= prismaSetupFn.importIdentifier =}();
{=/ prismaSetupFn.isDefined =}
{=^ prismaSetupFn.isDefined =}
const dbClient: PrismaClientResolved = InternalPrismaClient();
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
export type PrismaClient = typeof dbClient;

// PUBLIC API
export default dbClient;
