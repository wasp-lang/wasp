{{={= =}=}}
{=# areThereAnyEntitiesDefined =}
import { PrismaClient as InternalPrismaClient } from '@prisma/client'
import type { FromRegister } from '../types/register'
import { defineSyncStatefulResource } from './lifecycle/index.js'
{=# prismaSetupFn.isDefined =}
{=& prismaSetupFn.importStatement =}
{=/ prismaSetupFn.isDefined =}

// PUBLIC API
export type PrismaClient = ReturnType<RegisteredPrismaSetupFn>;

export type RegisteredPrismaSetupFn = FromRegister<'prismaSetupFn', () => InternalPrismaClient>;

// The client, and the pool of database connections it holds, outlives the code
// using it: in development, that code is replaced every time you change a file,
// and a client created per version of it would pile up connections.
const dbClient: PrismaClient = defineSyncStatefulResource('prisma', {
  {=# prismaSetupFn.isDefined =}
  create: () => {= prismaSetupFn.importIdentifier =}(),
  {=/ prismaSetupFn.isDefined =}
  {=^ prismaSetupFn.isDefined =}
  create: () => new InternalPrismaClient(),
  {=/ prismaSetupFn.isDefined =}
  dispose: (client) => client.$disconnect(),
});
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
