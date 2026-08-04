import { PrismaClient as InternalPrismaClient } from '@prisma/client'
import type { FromRegister } from '../types/register'
import { setUpPrisma as setUpPrisma_ext } from 'virtual:wasp/user/features/db/prisma'

// PUBLIC API
export type PrismaClient = ReturnType<RegisteredPrismaSetupFn>;

export type RegisteredPrismaSetupFn = FromRegister<'prismaSetupFn', () => InternalPrismaClient>;

const dbClient: PrismaClient =  setUpPrisma_ext();

// PUBLIC API
export default dbClient;
