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

// PUBLIC API
export default dbClient;
