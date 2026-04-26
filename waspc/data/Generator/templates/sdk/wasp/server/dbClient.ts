{{={= =}=}}
{=# areThereAnyEntitiesDefined =}
import { PrismaClient as InternalPrismaClient } from '@prisma/client'
import { FromRegistry } from "../types";

type UserPrismaSetupFn = FromRegistry<"prismaSetupFn", () => InternalPrismaClient>;
export type PrismaClientResolved = ReturnType<UserPrismaSetupFn>;

{=# prismaSetupFn.isDefined =}
{=& prismaSetupFn.importStatement =}
const dbClient: PrismaClientResolved =  {= prismaSetupFn.importIdentifier =}();
{=/ prismaSetupFn.isDefined =}
{=^ prismaSetupFn.isDefined =}
const dbClient: PrismaClientResolved = new InternalPrismaClient();
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
