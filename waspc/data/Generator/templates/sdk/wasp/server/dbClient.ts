{{={= =}=}}
{=# areThereAnyEntitiesDefined =}
import { PrismaClient as InternalPrismaClient } from '@prisma/client'
import type { FromRegister } from '../types/register'
import { getServerPrismaSetupFn } from './runtime.js'

// PUBLIC API
export type PrismaClient = ReturnType<RegisteredPrismaSetupFn>;

export type RegisteredPrismaSetupFn = FromRegister<'prismaSetupFn', () => InternalPrismaClient>;

const prismaSetupFn = getServerPrismaSetupFn();
const dbClient: PrismaClient = prismaSetupFn
  ? (prismaSetupFn() as PrismaClient)
  : new InternalPrismaClient();
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
