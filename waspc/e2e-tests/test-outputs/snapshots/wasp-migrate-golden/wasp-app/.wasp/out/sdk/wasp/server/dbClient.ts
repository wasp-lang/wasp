import { PrismaClient as InternalPrismaClient } from '@prisma/client'
import type { FromRegister } from '../types/register'
import { defineSyncStatefulResource } from './lifecycle/index.js'

// PUBLIC API
export type PrismaClient = ReturnType<RegisteredPrismaSetupFn>;

export type RegisteredPrismaSetupFn = FromRegister<'prismaSetupFn', () => InternalPrismaClient>;

// The client, and the pool of database connections it holds, outlives the code
// using it: in development, that code is replaced every time you change a file,
// and a client created per version of it would pile up connections.
const dbClient: PrismaClient = defineSyncStatefulResource('prisma', {
  create: () => new InternalPrismaClient(),
  dispose: (client) => client.$disconnect(),
});

// PUBLIC API
export default dbClient;
